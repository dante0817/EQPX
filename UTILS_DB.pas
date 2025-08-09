unit UTILS_DB;

interface

uses
  Math, ComCtrls, Grids, System.SysUtils, System.Classes, Data.DB, Data.SqlExpr, FireDAC.Comp.Client,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys.SQLite, FireDAC.DApt, FireDAC.Stan.ExprFuncs;

function FirstStationLatLon(StaName: string; ProgramPath: string): string;
procedure BrowseEarthquakeData(const Year, Month, Day: Integer; ListView: TListView; ProgramPath: string; MaxMagnitude: Double);

implementation

uses EQPX_1;

function FirstStationLatLon(StaName: string; ProgramPath: string): string;
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  DatabasePath: string;
  xLat, xLon, s: string;
begin
  s := '';

  // Initialize FireDAC connection and query
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
    // Construct the database path
    DatabasePath := ProgramPath + 'PHILSTA.sdb';

    // Setup SQLite connection
    FDConnection.DriverName := 'SQLite';
    FDConnection.Params.Database := DatabasePath;
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    FDQuery.Connection := FDConnection;

    // SQL query to fetch latitude and longitude of the specified station
    FDQuery.SQL.Text := 'SELECT Latitude, Longitude FROM Stations WHERE station_code = :StaName LIMIT 1';
    FDQuery.ParamByName('StaName').AsString := StaName;

    // Execute the query
    FDQuery.Open;

    // Check if a result was found
    if not FDQuery.Eof then
    begin
      xLat := Trim(FormatFloat('00.00', RoundTo(FDQuery.FieldByName('Latitude').AsFloat, -2)));
      xLon := Trim(FormatFloat('000.00', RoundTo(FDQuery.FieldByName('Longitude').AsFloat, -2)));

      s := xLat + ' ' + xLon + ' 000.0';

//      frmMain.ledLat.Text := xLat;
//      frmMain.ledLon.Text := xLon;
    end
    else
    begin
      s := 'Station not found';
    end;

  finally
    // Cleanup
    FDQuery.Free;
    FDConnection.Free;
  end;

  Result := s;
end;

procedure BrowseEarthquakeData(const Year, Month, Day: Integer; ListView: TListView; ProgramPath: string; MaxMagnitude: Double);
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  SQLQuery: string;
  DateFilter: string;
  DatabasePath: string;
  DateTimeValue, IntensityAccount: string;
  Magnitude, Latitude, Longitude: Double;
  Depth: Integer;  // Depth as integer
  ListItem: TListItem;
  DatePart, TimePart: string;
begin
  FDConnection := TFDConnection.Create(nil);
  FDQuery := TFDQuery.Create(nil);
  try
    // Construct the database path
    DatabasePath := ProgramPath + 'DB\EQCatalogue_web.sdb';

    // Setup SQLite connection
    FDConnection.DriverName := 'SQLite';
    FDConnection.Params.Database := DatabasePath;
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    FDQuery.Connection := FDConnection;

    // Format date filter
    DateFilter := Format('%.4d-%.2d-%.2d', [Year, Month, Day]);

    // SQL query to filter data by date, magnitude and extract intensity
    SQLQuery := 'SELECT datetime, ms, latitude, longitude, depth, intensity_account FROM EarthquakeData ' +
                'WHERE datetime LIKE :DateFilter AND ms < :MaxMagnitude';
    FDQuery.SQL.Text := SQLQuery;
    FDQuery.ParamByName('DateFilter').AsString := DateFilter + '%';
    FDQuery.ParamByName('MaxMagnitude').AsFloat := MaxMagnitude;

    // Execute the query
    FDQuery.Open;

    // Clear the ListView before populating new data
    ListView.Items.Clear;

    // Populate the ListView with data
    while not FDQuery.Eof do
    begin
      DateTimeValue := FDQuery.FieldByName('datetime').AsString;
      Magnitude := FDQuery.FieldByName('ms').AsFloat;
      Latitude := FDQuery.FieldByName('latitude').AsFloat;    // Fetch latitude
      Longitude := FDQuery.FieldByName('longitude').AsFloat;  // Fetch longitude
      Depth := FDQuery.FieldByName('depth').AsInteger;        // Fetch depth as integer
      IntensityAccount := FDQuery.FieldByName('intensity_account').AsString;  // Intensity description

      // Split the datetime string into date and time parts
      DatePart := Copy(DateTimeValue, 1, 10);  // Extract 'YYYY-MM-DD'
      TimePart := Copy(DateTimeValue, 12, 8);  // Extract 'HH:NN:SS'

      // Add a new item to the ListView
      ListItem := ListView.Items.Add;
      ListItem.Caption := '';  // Leave the checkbox column blank
      ListItem.SubItems.Add(DatePart);  // Set the date in the second column
      ListItem.SubItems.Add(TimePart);  // Set the time in the third column
      ListItem.SubItems.Add(FormatFloat('0.0', Magnitude));  // Set the magnitude in the fourth column
      ListItem.SubItems.Add(FormatFloat('0.000', Latitude));  // Latitude with 3 decimal places
      ListItem.SubItems.Add(FormatFloat('0.000', Longitude)); // Longitude with 3 decimal places
      ListItem.SubItems.Add(IntToStr(Depth));                 // Depth as integer
      ListItem.SubItems.Add(IntensityAccount);                // Intensity account (description)

      // Check the checkbox by default
      ListItem.Checked := True;  // This line ensures the checkbox is checked

      FDQuery.Next;
    end;

  finally
    FDQuery.Free;
    FDConnection.Free;
  end;
end;

end.
