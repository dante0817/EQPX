unit EQPlot;

interface

uses
  StrUtils, DateUtils, SysUtils, Classes;
//  Math, Classes, Grids, Windows, Vcl.Forms, Vcl.CheckLst;

procedure Update_resPS_delta;
procedure ExtractResult_hdg(EQP_result : string);

function Highest_PS(P_S: Char) : Integer;
procedure MainReplot;
procedure Replot_orig;

implementation

uses  EQPX_1, UTILS_DateTime, UTILS, UTILS2;

procedure Update_resPS_delta;
var
  i, j, row: Integer;
  mmoLines: TStringList;
  sta, resP, resS, sDelta: string;

  found: Boolean;
  procedure ExtractColumns(const input: string);
  begin
    sta  := Trim(Copy(input, 1, 4));
    resp := Trim(Copy(input, 23, 5));
    ress := Trim(Copy(input, 29, 5));
    sdelta :=Trim(Copy(input, 17, 5)); // Extract delta
  end;
begin
  mmoLines := TStringList.Create;
  try
    // Split the memo's text into lines
    mmoLines.Text := frmMain.mmoResult.Text;
    // Iterate through the lines of the memo's text, starting at line 9
    for i := 8 to mmoLines.Count - 1 do
    begin
      // Extract columns based on fixed positions
      ExtractColumns(mmoLines[i]);
      // Search for the existing STA value in the stringgrid
      found := False;
      for j := 0 to frmMain.sgMainData.RowCount - 1 do
      begin
        if frmMain.sgMainData.Cells[0, j] = sta then
        begin
          found := True;
          row := j;
          break;
        end;
      end;
      // If the STA value is found, update the RESp and RESs columns
      if found then
      begin
        frmMain.sgMainData.Cells[2, row] := resP; // RESp
        frmMain.sgMainData.Cells[5, row] := resS; // RESs
        frmMain.sgMainData.Cells[8, row] := sDelta; // Delta
      end;
    end;
  finally
    mmoLines.Free;
  end;
end;

procedure ExtractResult_hdg(EQP_result : string);
var
  i : integer;
  s, x : string;
  values: TArray<string>;
  hr, min, sec : string;
  LAT, LON, DEP : string;
  year, mon, day : string;
  SecondsFloat: Single;
  Seconds, Milliseconds: Word;
begin
  frmMain.ledRMS.Text := RightStr(EQP_result,4);

  s := EQP_result;

  values := s.Split([' '], TStringSplitOptions.ExcludeEmpty);

  LAT := FormatFloat('00.00', StrToFloat(values[1]));   // LAT
  LON := FormatFloat('000.00', StrToFloat(values[2]));  // LON
  DEP := FormatFloat('000.0', StrToFloat(values[3]));   // DEP

  hr  := FormatFloat('00', StrToFloat(values[4]));      //  hr
  min := FormatFloat('00', StrToFloat(values[5]));      //  min
  sec := FormatFloat('00.00', StrToFloat(values[6]));   //  sec

  //----- extract date-time to format
  //----- fs.ShortDateFormat := 'yyyy-MM-dd' + fs.LongTimeFormat := 'hh:mm:ss.zzzz';
  year := RightStr(frmMain.mmoResult.Lines[0], 2); // year = YYYY
  year := '20' + FormatFloat('00', StrToFloat(year));

  mon := LeftStr(frmMain.mmoResult.Lines[0], 2);
  mon := FormatFloat('00', StrToFloat(mon)); // year + month
  s := S +'-'+ FormatFloat('00', StrToFloat(mon)); // year + month

  day := copy(frmMain.mmoResult.Lines[0], 3, 2);  // day
  day := FormatFloat('00', StrToFloat(day));    // year + month + day
  s := s +'-'+ FormatFloat('00', StrToFloat(day));    // year + month + day

  s := year +'-'+ mon +'-'+ day +' '+ hr +':'+ min +':'+ sec ;
  s := UTILS_DateTime.ExtractDateTime_heading(s); // datetime

  frmMain.sgMainData.Cells[0,0] := s +' '+ LAT +' '+ LON +' '+ DEP;

  // update values of Outputs: datetime, lat, Lon, Dep
  frmMain.ledLat.Text     := FormatFloat('000.000', StrToFloat(values[1]));   // LAT
  frmMain.ledLon.Text     := FormatFloat('000.000', StrToFloat(values[2]));  // LON
  frmMain.ledDep.Text     := FormatFloat('000.0', StrToFloat(values[3]));   // DEP

  frmMain.ledEQ_name.Text := Year +'_'+ Mon + Day +'_'+ Hr + Min;
  frmMain.dtpEQDateTime.DateTime := EncodeDateTime(StrToInt(Year), StrToInt(Mon), StrToInt(Day), StrToInt(Hr), StrToInt(Min), 0, 0);
end;

//=============================
function Highest_PS(P_S: Char) : Integer;
var
  i, highestValueRow: Integer;
  highestValue, currentValue: Double;
  columnValue, columnMarker: Integer;
  ctr: Integer;
begin
  Result := -1; // Default return value indicating no row found.
  // Assign column numbers based on the data type.
  if P_S = 'P' then
  begin
    columnValue := 2;
    columnMarker := 3;
  end
  else // Assume 'S'
  begin
    columnValue := 5;
    columnMarker := 6;
  end;
  // Exit if there is no data to process.
  if frmMain.sgMainData.RowCount < 2 then
    Exit;
  // Initialize the highest value as 0 and the highest value row as 1.
  highestValue := 0.0;
  highestValueRow := 1;
  // Get the row count and iterate through the rows of the grid.
  ctr := frmMain.sgMainData.RowCount;
  for i := 1 to ctr - 1 do
  begin
    // Continue to the next iteration if the current cell in the marker column has an asterisk.
    if (frmMain.sgMainData.Cells[columnMarker, i]  = '*') then continue;
    // Get the absolute value of the current cell in the value column, using the highest value as a default.
    currentValue := Abs(StrToFloatDef(frmMain.sgMainData.Cells[columnValue, i], highestValue ));
    // Check if the current value is greater than the highest value found so far.
    if currentValue > highestValue then
    begin
      // If so, update the highest value and the highest value row.
      highestValue := currentValue;
      highestValueRow := i;
    end;
  end;
  // Return the row number of the highest value only if the highest value is greater than or equal to 1.
  if highestValue >= 1 then
    Result := highestValueRow
  else
    Result := -1; // Return -1 if the highest value is less than 1.
end;

procedure MainReplot;
var
  Counter: Integer;
  x : boolean;
begin
  if not UTILS2.Remaining_PS then exit;

//------------------ RMS >= 10.0
  while UTILS2.CheckValuesAndAddAsterisk_S(10.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

//------------------ RMS >= 5.0
  while UTILS2.CheckValuesAndAddAsterisk_S(5.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

//------------------ RMS >= 4.0
  while UTILS2.CheckValuesAndAddAsterisk_S(4.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

//------------------ RMS >= 3.0
  while UTILS2.CheckValuesAndAddAsterisk_S(3.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

  if UTILS2.CheckValuesAndAddAsterisk_P(3.0) then
    frmMain.Plot_againClick(nil);

//------------------ RMS >= 2.0
  while UTILS2.CheckValuesAndAddAsterisk_S(2.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

  if UTILS2.CheckValuesAndAddAsterisk_P(2.0) then
    frmMain.Plot_againClick(nil);

//------------------ RMS >= 1.0
  while UTILS2.CheckValuesAndAddAsterisk_S(1.0) do begin
    if not UTILS2.Remaining_PS then exit;
    frmMain.Plot_againClick(nil);
  end;

  if UTILS2.CheckValuesAndAddAsterisk_P(1.0) then
    frmMain.Plot_againClick(nil);
end;

procedure Replot_orig;
var
  numP, numS : integer;
  i, row : integer;
  P_error_min : single;
begin
  P_error_min := StrToFloat(frmMain.ledError_Pmin.Text);

  row := UTILS2.Highest_PS('P'); // P >= 1
  if (row <>-1) then
  for i := 1 to frmMain.sgMainData.RowCount-1 do begin
    if frmMain.sgMainData.Cells[2, row] ='' then break;
  end;
//    if (Abs(StrToFloatDef(frmMain.sgMainData.Cells[2, row], 0)) >= 1.5) then begin
    if (Abs(StrToFloatDef(frmMain.sgMainData.Cells[2, row], 0)) >= P_error_min) then begin

//  while (frmMain.sgMainData.Cells[2, row] <> '') and (Abs(StrToFloatDef(frmMain.sgMainData.Cells[2, row], 0)) >= 1.5) do begin
//  while (Abs(StrToFloatDef(frmMain.sgMainData.Cells[2, row], 0)) >= StrToFloatDef(frmMain.ledError_Pmin.Text, 0)) do begin
    if (row <> -1) then begin
      frmMain.sgMainData.Cells[3, row] := '*';
      frmMain.Plot_againClick(nil);
    end;

//    if StrToInt(frmMain.sgMainData.Cells[6, 0]) >= 1 then   // check for S RMS >= 1.0
    row := UTILS2.Highest_PS('S'); // S >= 1
    if (row <> -1) then begin
//    if (row <> -1) and (Abs(StrToFloatDef(frmMain.sgMainData.Cells[5, row], 0)) >= 1) then begin
      frmMain.sgMainData.Cells[6, row] := '*';
      frmMain.Plot_againClick(nil);
    end;

    row := UTILS2.Highest_PS('P');
  end;

  frmMain.edtError_P.Text := IntToStr(UTILS.CountGreaterEqualOne('P')); // Count the values greater than or equal to 1.0 for 'P'
  frmMain.edtError_S.Text := IntToStr(UTILS.CountGreaterEqualOne('S')); // Count the values greater than or equal to 1.0 for 'S'

  while (StrToInt(frmMain.edtError_P.Text) >= 1) or (StrToInt(frmMain.edtError_S.Text) >= 1) do begin
//  while (StrToInt(frmMain.sgMainData.Cells[5, 0]) >= 1) or (StrToInt(frmMain.sgMainData.Cells[6, 0]) >= 1) do begin
    row := UTILS2.Highest_PS('P'); // For the first case (P)
    if (row <> -1) then begin
      frmMain.sgMainData.Cells[3, row] := '*';
      frmMain.Plot_againClick(nil);
    end;

//    if StrToInt(frmMain.sgMainData.Cells[6, 0]) >= 1 then   // check for S RMS >= 1.0
    row := UTILS2.Highest_PS('S'); // For the second case (S)
    if (row <> -1) then begin
      frmMain.sgMainData.Cells[6, row] := '*';
      frmMain.Plot_againClick(nil);
    end;
  end;
end;

end.
