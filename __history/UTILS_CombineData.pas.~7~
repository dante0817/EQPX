unit UTILS_CombineData;

interface

uses
  Vcl.Grids, SysUtils, StrUtils, Vcl.StdCtrls, Classes;

procedure SortAtlasDataByPTime(Grid: TStringGrid);
procedure CombineAndUpdateAtlasData(SourceGrid, TargetGrid: TStringGrid);

implementation

uses EQPX_1;

procedure SortAtlasDataByPTime(Grid: TStringGrid);
var
  i, j: Integer;
  tempRow: TStringList;
begin
  // Create a temporary TStringList to hold row values during swapping
  tempRow := TStringList.Create;
  try
    // Use a simple bubble sort algorithm to sort by P Time (ascending order)
    for i := 1 to Grid.RowCount - 2 do
    begin
      for j := i + 1 to Grid.RowCount - 1 do
      begin
        // Compare P Time values between row i and row j
        if Grid.Cells[4, i] > Grid.Cells[4, j] then
        begin
          // Swap rows i and j
          tempRow.Assign(Grid.Rows[i]);
          Grid.Rows[i].Assign(Grid.Rows[j]);
          Grid.Rows[j].Assign(tempRow);
        end;
      end;
    end;
  finally
    tempRow.Free;
  end;
end;

procedure CombineAndUpdateAtlasData(SourceGrid, TargetGrid: TStringGrid);
var
  i, j, CombinedRowCount: Integer;
  stationValuePhase, stationValueAtlas: string;
  found: Boolean;
begin
  // Step 1: Initialize CombinedRowCount with the current row count of TargetGrid
  CombinedRowCount := TargetGrid.RowCount;

  // Step 2: Iterate through each row in SourceGrid to insert or update data
  for i := 1 to SourceGrid.RowCount - 1 do
  begin
    // Get the station value from SourceGrid
    stationValuePhase := SourceGrid.Cells[0, i]; // Assuming station column is at index 0

    found := False;

    // Step 3: Check if the station already exists in TargetGrid
    for j := 1 to TargetGrid.RowCount - 1 do
    begin
      stationValueAtlas := TargetGrid.Cells[1, j]; // Assuming station column is at index 1 in TargetGrid

      if stationValuePhase = stationValueAtlas then
      begin
        // Station found, update the existing row
        TargetGrid.Cells[3, j] := SourceGrid.Cells[1, i];  // P Phase
        TargetGrid.Cells[7, j] := SourceGrid.Cells[2, i];  // Polarity
        TargetGrid.Cells[4, j] := SourceGrid.Cells[3, i];  // P arrival to P Time
        TargetGrid.Cells[5, j] := SourceGrid.Cells[4, i];  // S Phase
        TargetGrid.Cells[6, j] := SourceGrid.Cells[5, i];  // S arrival to S Time
        TargetGrid.Cells[12, j] := SourceGrid.Cells[7, i]; // Duration
        found := True;
        Break; // Stop searching once the station is found and updated
      end;
    end;

    // Step 4: If the station was not found, add it as a new row in TargetGrid
    if not found then
    begin
      TargetGrid.RowCount := CombinedRowCount + 1; // Increase row count to add a new row
      TargetGrid.Cells[1, CombinedRowCount] := stationValuePhase;           // Station
      TargetGrid.Cells[3, CombinedRowCount] := SourceGrid.Cells[1, i];  // P Phase
      TargetGrid.Cells[7, CombinedRowCount] := SourceGrid.Cells[2, i];  // Polarity
      TargetGrid.Cells[4, CombinedRowCount] := SourceGrid.Cells[3, i];  // P arrival to P Time
      TargetGrid.Cells[5, CombinedRowCount] := SourceGrid.Cells[4, i];  // S Phase
      TargetGrid.Cells[6, CombinedRowCount] := SourceGrid.Cells[5, i];  // S arrival to S Time
      TargetGrid.Cells[12, CombinedRowCount] := SourceGrid.Cells[7, i]; // Duration

      // Update the row count tracker
      Inc(CombinedRowCount);
    end;
  end;

  // Step 5: Sort the data in TargetGrid by P Time after all updates and insertions
  SortAtlasDataByPTime(TargetGrid);
end;

end.

