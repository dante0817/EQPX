unit UTILS_stations;

interface

uses
  FireDAC.Comp.Client, Vcl.Grids;

procedure LoadStationsToGrid(sgSOEPD_stations: TStringGrid; const DBPath: string);

implementation

procedure LoadStationsToGrid(sgSOEPD_stations: TStringGrid; const DBPath: string);
var
  FDConnection: TFDConnection;
  FDQuery: TFDQuery;
  i, j: Integer;
begin
  // Initialize connection to SQLite DB
  FDConnection := TFDConnection.Create(nil);
  try
    FDConnection.DriverName := 'SQLite';
    FDConnection.Params.Database := DBPath; // Set the database path dynamically
    FDConnection.LoginPrompt := False;
    FDConnection.Connected := True;

    // Create query to select all data from stations
    FDQuery := TFDQuery.Create(nil);
    try
      FDQuery.Connection := FDConnection;

      // Set RowsetSize to fetch all rows at once
      FDQuery.FetchOptions.RowsetSize := -1;  // Fetch all rows

      // SQL query to fetch all stations
      FDQuery.SQL.Text := 'SELECT * FROM stations';
      FDQuery.Open;

      // Set up StringGrid column headers
      sgSOEPD_stations.RowCount := 1; // Initially, only header row
      sgSOEPD_stations.ColCount := FDQuery.FieldCount;

      // Set up column headers in the first row (row 0)
      for i := 0 to FDQuery.FieldCount - 1 do
      begin
        sgSOEPD_stations.Cells[i, 0] := FDQuery.Fields[i].FieldName; // Header
      end;

      // Populate StringGrid with data
      i := 1; // Start at row 1 since row 0 is header
      while not FDQuery.Eof do
      begin
        sgSOEPD_stations.RowCount := sgSOEPD_stations.RowCount + 1;  // Add a new row for each record
        for j := 0 to FDQuery.FieldCount - 1 do
        begin
          sgSOEPD_stations.Cells[j, i] := FDQuery.Fields[j].AsString;
        end;
        Inc(i);
        FDQuery.Next;
      end;

    finally
      FDQuery.Free;
    end;

  finally
    FDConnection.Free;
  end;
end;

end.

