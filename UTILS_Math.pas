unit UTILS_Math;

interface

uses
  SysUtils, System.StrUtils, DateUtils, Math;

procedure RemoveOutliersAndComputeMean;
function ComputeMagnitude(const Station: string; Duration, Delta: Single): Single;

function ComputeCoda(sDateTime_P, sDateTime_C: string): string;
function Haversine(lat1, lon1, lat2, lon2: Double): Double;

implementation

uses EQPX_1;


procedure RemoveOutliersAndComputeMean;
var
  i, totalCount, inlierCount: Integer;
  magValue, sumMag, mean, variance, stdDev, upperBound, lowerBound: Single;
begin
  with frmMain.sgMainData do
  begin
    // --- Step 1: Compute raw mean of all valid magnitudes ---
    sumMag := 0;
    totalCount := 0;
    for i := 1 to RowCount - 1 do
      if TryStrToFloat(Cells[9, i], magValue) then
      begin
        Inc(totalCount);
        sumMag := sumMag + magValue;
      end;

    if totalCount = 0 then
    begin
      frmMain.ledMagnitude.Text := '0.00';
      Exit;
    end;

    mean := sumMag / totalCount;

    // --- Step 2: Compute standard deviation ---
    variance := 0;
    for i := 1 to RowCount - 1 do
      if TryStrToFloat(Cells[9, i], magValue) then
        variance := variance + Sqr(magValue - mean);

    stdDev := Sqrt(variance / totalCount);
    upperBound := mean + stdDev;
    lowerBound := mean - stdDev;

    // --- Step 3: Compute filtered (inlier) mean ---
    sumMag := 0;
    inlierCount := 0;
    for i := 1 to RowCount - 1 do
      if TryStrToFloat(Cells[9, i], magValue) and
         (magValue >= lowerBound) and (magValue <= upperBound) then
      begin
        Inc(inlierCount);
        sumMag := sumMag + magValue;
      end;

    if inlierCount > 0 then
      mean := sumMag / inlierCount
    else
      mean := 0;

    // --- Step 4: Display result in ledMagnitude ---
    frmMain.ledMagnitude.Text := FormatFloat('0.00', mean);
  end;
end;

function ComputeMagnitude(const Station: string; Duration, Delta: Single): Single;
var
  Mag: Single;
begin
  // Skip invalid or rejected rows (e.g., station marked with *)
  if (Pos('*', Station) > 0) or (Duration = 0) then
  begin
    Result := 0.0;
    Exit;
  end;

  // Compute magnitude depending on distance
  if Delta > 4.5 then
    Mag := -3.68 + 3.24 * Log10(Duration) + 9.07 * 0.0001 * Delta * 111.1
  else
    Mag := -3.49 + 3.24 * Log10(Duration);

  Result := Mag;
end;

function Haversine(lat1, lon1, lat2, lon2: Double): Double;
const
  R = 6371; // Earth's radius in kilometers
var
  dLat, dLon, a, c: Double;
begin
  dLat := DegToRad(lat2 - lat1);
  dLon := DegToRad(lon2 - lon1);

  a := Sin(dLat / 2) * Sin(dLat / 2) + Cos(DegToRad(lat1)) * Cos(DegToRad(lat2)) * Sin(dLon / 2) * Sin(dLon / 2);
  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));

  Result := R * c; // Distance in kilometers
end;

function ComputeCoda(sDateTime_P, sDateTime_C: string): string;
var
  yrP, monP, dayP: integer;
  yrC, monC, dayC: integer;
  dtDateTime_P, dtDate_P, dtTime_P: TDateTime;
  dtDateTime_C, dtDate_C, dtTime_C: TDateTime;
  s: string;
begin
  // Extract time and date from P phase
  s := Trim(Copy(sDateTime_P, 12, 8)); // Time part for P phase
  dtTime_P := StrToTime(s);

  yrP := StrToInt(LeftStr(sDateTime_P, 4));  // Year
  monP := StrToInt(Copy(sDateTime_P, 6, 2)); // Month
  dayP := StrToInt(Copy(sDateTime_P, 9, 2)); // Day
  dtDate_P := EncodeDate(yrP, monP, dayP);   // Date
  dtDateTime_P := dtDate_P + dtTime_P;       // Full DateTime for P phase

  // Extract time and date from Coda time
  s := Trim(Copy(sDateTime_C, 12, 8)); // Time part for Coda
  dtTime_C := StrToTime(s);

  yrC := StrToInt(LeftStr(sDateTime_C, 4));  // Year
  monC := StrToInt(Copy(sDateTime_C, 6, 2)); // Month
  dayC := StrToInt(Copy(sDateTime_C, 9, 2)); // Day
  dtDate_C := EncodeDate(yrC, monC, dayC);   // Date
  dtDateTime_C := dtDate_C + dtTime_C;       // Full DateTime for Coda

  // Calculate the seconds difference between P phase and Coda time
  Result := FloatToStr(SecondsBetween(dtDateTime_C, dtDateTime_P));
end;

end.
