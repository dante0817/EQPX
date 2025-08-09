unit EQPlotter_data;

interface

uses
  DateUtils, System.StrUtils, Math, ComCtrls, Grids, System.SysUtils, System.Classes, Data.DB, Data.SqlExpr, FireDAC.Comp.Client,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys.SQLite, FireDAC.DApt, FireDAC.Stan.ExprFuncs;

procedure PHILSTA_DAT(sPHILSTA_dat: string; ProgramPath: string);
procedure PLOT_dat(sPLOT_dat : string);
procedure EQData(sgData : TStringGrid);

function ExtractDateTime(sDateTime : string) : string;
function CreatePlotDatFirstLine(sDateTime : string) : string;
procedure CreatePlotDat(sgAtlasData_Final: TStringGrid);

implementation

uses Utils_DB, UTILS, EQPX_1;

procedure PHILSTA_DAT(sPHILSTA_dat: string; ProgramPath: string);
var
  i: Integer;
  s, StaName: string;
  philstaList: TStringList;
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  DatabasePath, xLat, xLon, xElev: string;
begin
  philstaList := TStringList.Create;
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
    // Set up the database connection
    DatabasePath := IncludeTrailingPathDelimiter(ProgramPath) + 'DB\PHILSTA.sdb';
    FDConnection.DriverName := 'SQLite';
    FDConnection.Params.Database := DatabasePath;
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    FDQuery.Connection := FDConnection;

    for i := 1 to frmMain.sgMainData.RowCount - 1 do
    begin
      StaName := Trim(frmMain.sgMainData.Cells[0, i]);
      if StaName = '' then Break;

      // Query the database for latitude and longitude
      FDQuery.SQL.Text := 'SELECT station_code, latitude, longitude, elevation FROM Stations WHERE station_code = :StaName LIMIT 1';
      FDQuery.ParamByName('StaName').AsString := Copy(StaName, 1, 3);
      FDQuery.Open;

      if not FDQuery.Eof then
      begin
        xLat := FormatFloat('00.000', FDQuery.FieldByName('latitude').AsFloat);
        xLon := FormatFloat('000.000', FDQuery.FieldByName('longitude').AsFloat);
        xElev := FormatFloat('000', FDQuery.FieldByName('elevation').AsInteger);
        s := StaName + '     ' + xLat + '   ' + xLon + '   ' +xElev;

        // Add the station data to the list
        philstaList.Add(s);
      end;

      FDQuery.Close;
    end;

    // Save the list to the specified file
    philstaList.SaveToFile(IncludeTrailingPathDelimiter(ProgramPath) + 'Plotter\' + sPHILSTA_dat);

  finally
    // Cleanup
    philstaList.Free;
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

//-------------------------
procedure PLOT_dat(sPLOT_dat: string);
var
  i: Integer;
  s, x: string;
  durationVal: Integer;
  listPlot_dat: TStringList;
begin
  listPlot_dat := TStringList.Create;
  try
    with frmMain do
    begin
      // Header line
      s := sgMainData.Cells[0, 0];
      listPlot_dat.Add(s);

      // Data lines
      for i := 1 to sgMainData.RowCount - 1 do
      begin
        // Skip excluded picks
        if Trim(sgMainData.Cells[3, i]) = '*' then
          Continue;

        // Station code
        x := sgMainData.Cells[0, i];
        s := x + '    ';

        // P arrival
        if Trim(sgMainData.Cells[3, i]) = '' then
          s := s + sgMainData.Cells[1, i] + '   ';

        // S arrival
        if (Trim(sgMainData.Cells[4, i]) <> '') and (Trim(sgMainData.Cells[6, i]) = '') then
          s := s + sgMainData.Cells[4, i]
        else
          s := s + '       ';

        // CODA / Duration → truncate to integer
        if Trim(sgMainData.Cells[7, i]) <> '' then
        begin
          durationVal := Trunc(StrToFloatDef(sgMainData.Cells[7, i], 0));
          s := s + '   ' + IntToStr(durationVal);
        end;

        listPlot_dat.Add(s);
      end;

      // End marker
      listPlot_dat.Add('XXX');

      // Save to file
      listPlot_dat.SaveToFile(lblProgramPath.Caption + 'Plotter\' + sPLOT_dat);
    end;
  finally
    listPlot_dat.Free;
  end;
end;

procedure EQData(sgData : TStringGrid);
var
  i, j, iNumS :integer;
  xfloat : double;
  xChannel, xStaName, s, x, y, xLat, xLon : string;
begin
//    frmMain.sgResult.RowCount := frmMain.sgAtlasPick.RowCount;
//  sgData.RowCount := frmMain.sgAtlasPick.RowCount;

  x := EQPlotter_data.CreatePlotDatFirstLine(frmMain.sgAtlasData_Final.Cells[4,1]);

//  frmMain.ledFileName.Text := frmMain.sgAtlasPick.Cells[4,4];

  xStaName := frmMain.sgAtlasData_Final.Cells[1,1]; // first Station
  xStaName := LeftStr(xStaName, 3);

  s := x +' '+ Utils_DB.FirstStationLatLon(xStaName, frmMain.lblProgramPath.Caption + 'DB\');  // extract First Station LatLon

//  frmMain.mmoPlot_dat.Lines.Add(s);
  sgData.Cells[0,0] := s;   // write PLOT.DATA heading: 0214190237 47.5 08.15 124.03 000.0

  iNumS := 0;
  for i := 1 to frmMain.sgAtlasData_Final.RowCount-1 do begin
    if Trim(frmMain.sgAtlasData_Final.Cells[1, i]) = '' then break;

    //----- station
    x := LeftStr(frmMain.sgAtlasData_Final.Cells[1, i], 3);
    s := x +'    ';
    sgData.Cells[0,i] := x;

    //----- P minute + second
    x := Utils.ExtractDateTime(frmMain.sgAtlasData_Final.Cells[4, i]);
    s := s + x;
    sgData.Cells[1,i] := x;

    //----- S minute + second
    if Trim(frmMain.sgAtlasData_Final.Cells[6, i]) <> ''
    then begin
      x := Utils.ExtractDateTime(frmMain.sgAtlasData_Final.Cells[6, i]);
      s := s + x;
      sgData.Cells[4,i] := x;
      inc(iNumS);
    end
    else s := s + '       ';

    //----- CODA
    if Trim(frmMain.sgAtlasData_Final.Cells[12, i]) <> '' then begin
      x := frmMain.sgAtlasData_Final.Cells[12, i];
      s := s +'   '+ x;
      sgData.Cells[7,i] := x;
    end;
  end;

  //------ Set the final rowcount
  sgData.RowCount := i;

  frmMain.edtTotal_P.Text := IntToStr(i-1);    //  num P (Station)
  frmMain.edtTotal_S.Text := IntToStr(iNumS);  //  num S

end;

function ExtractDateTime(sDateTime : string) : string;
var
  wX, wYr, wMon, wDay, wHr, wMin, wSec, wMSec : Word;
  s, x : string;
  OldShortDateFormat: string;
  OldDateSeparator: Char;
  dt: TDateTime;
  fs: TFormatSettings;
  i : integer;
begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';

  fs.ShortDateFormat := 'yyyy-MM-dd';
  fs.LongTimeFormat := 'hh:mm:ss.zzzz';

  dt := StrToDateTime(sDateTime, fs);
  DecodeDate(dt, wYr, wMon, wDay);
  DecodeTime(dt, wHr, wMin, wSec, wMSec);

//  frmMain.dtpEQ_datetime_first_GMT.DateTime := dt;    // ----------------- update datetimePicker

  if wMin < 10 then begin
    wHr := wHr - 1;
    wMin := wMin + 60;
  end;

//    Result := Format('%.*d%.*d%.*d%.*d%.*d %.1f', [2, wMon, 2, wDay, 2, wYr mod 100, 2, wHr, 2, wMin, StrToFloat(frmMain.sgAtlasPick.Cells[7, 2])]);

    s := FormatFloat('00', wMon); // month
    s := s + FormatFloat('00', wDay); // day
    s := s + FormatFloat('00', StrToInt(RightStr(IntToStr(wYr), 2))); // year

    s := s + FormatFloat('00', wHr); // hour
    s := s + FormatFloat('00', wMin); // minute
    x := RightStr(sDateTime, 5);
//    s := s +' '+ FormatFloat('00.0', StrToFloat(frmMain.sgAtlasPick.Cells[7,2])); // second
    s := s +' '+ FormatFloat('00.0', StrToFloat(x)); // second

  Result := s;
end;


function CreatePlotDatFirstLine(sDateTime : string) : string;
var
  wYr, wMon, wDay, wHr, wMin, wSec, wMSec : Word;
  dt: TDateTime;
  fs: TFormatSettings;
  x : string;

  DateTimeStr, LatitudeStr, LongitudeStr, DepthStr, MagnitudeStr: string;
  DateTimeValue: TDateTime;
  FormattedDateTime, SecondsStr: string;
  Year, Month, Day, Hour, Minute, Second, Millisecond: Word;
  FormatSettings: TFormatSettings;
begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';

  fs.ShortDateFormat := 'yyyy-MM-dd';
  fs.LongTimeFormat := 'hh:mm:ss.zzzz';

  dt := StrToDateTime(sDateTime, fs);
  DecodeDate(dt, wYr, wMon, wDay);
  DecodeTime(dt, wHr, wMin, wSec, wMSec);

  x := RightStr(sDateTime, 5);

//    Result := Format('%.*d%.*d%.*d%.*d%.*d %.1f', [2, wMon, 2, wDay, 2, wYr mod 100, 2, wHr, 2, wMin, StrToFloat(frmMain.sgAtlasPick.Cells[7, 2])]);

        // Format the DateTime into mmddyyhhmm (month, day, year, hour, minute)
    Result := Format('%.*d%.*d%.*d%.*d%.*d %.1f', [2, wMon, 2, wDay, 2, wYr mod 100, 2, wHr, 2, wMin, StrToFloat(x)]);

    //    FormattedDateTime := Format('%.2d%.2d%.2d%.2d%.2d ', [wMon, wDay, wYr mod 100, wHr, wMin], StrToFloat(x);

//    SecondsFraction := FormatFloat('00.0', Second + Millisecond / 1000);

    // Extract the seconds as a string with one decimal place (ss.s)
    SecondsStr := Format('%2.1f', [Millisecond / 1000]);
//    SecondsStr := Format('%2.1f', [Second + Millisecond / 1000]);

  // Construct the output line in the specified format
//  OutputLine := Format('%s %s %s %s %s', [FormattedDateTime, SecondsStr, LatitudeStr, LongitudeStr, DepthStr]);
end;

procedure CreatePlotDat(sgAtlasData_Final: TStringGrid);
var
  PlotFile: TextFile;
  PArrival, SArrival, CodaTime: Double;
  Station: string;
  PTime, STime, CodaDateTime: TDateTime;
  FormattedPTime, FormattedSTime, FormattedCoda: string;
  i: Integer;
begin
  // Assign file and rewrite it to create a new text file
  AssignFile(PlotFile, 'PLOT.DAT');
  Rewrite(PlotFile);

  try
    // Loop through the rows of the string grid to extract data
    for i := 1 to sgAtlasData_Final.RowCount - 1 do
    begin
      Station := sgAtlasData_Final.Cells[1, i];
      PTime := StrToDateTime(sgAtlasData_Final.Cells[4, i]);
      STime := StrToDateTimeDef(sgAtlasData_Final.Cells[6, i], 0);
      CodaDateTime := StrToDateTimeDef(sgAtlasData_Final.Cells[9, i], 0);
      CodaTime := StrToFloatDef(sgAtlasData_Final.Cells[10, i], 0);

      // Format the P, S and Coda times into seconds since start of the day
      PArrival := (HourOf(PTime) * 3600) + (MinuteOf(PTime) * 60) + SecondOf(PTime) + (MilliSecondOf(PTime) / 1000);
      SArrival := (HourOf(STime) * 3600) + (MinuteOf(STime) * 60) + SecondOf(STime) + (MilliSecondOf(STime) / 1000);
      FormattedPTime := FormatFloat('0000.00', PArrival);
      FormattedSTime := FormatFloat('0000.00', SArrival);
      FormattedCoda := FormatFloat('0000.00', CodaTime);

      // Writing to the PLOT.DAT file using the desired format
      WriteLn(PlotFile, Format('%s %s %s %s', [Station, FormattedPTime, FormattedSTime, FormattedCoda]));
    end;
  finally
    // Close the file
    CloseFile(PlotFile);
  end;
end;

end.
