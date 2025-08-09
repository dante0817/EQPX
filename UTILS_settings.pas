unit UTILS_settings;

interface

uses
  ComCtrls;  // Include ComCtrls to access TListView

// Declare the SetupListView procedure
procedure SetupListView(ListView: TListView);

implementation

procedure SetupListView(ListView: TListView);
begin
  ListView.Columns.Clear;  // Clear any existing columns

  // Add a small-width or invisible column for the checkboxes
  with ListView.Columns.Add do
  begin
    Caption := ''; // Leave the caption blank for the checkbox column
    Width := 20;   // Small width, or set to 0 to completely hide it
  end;

  // Add the columns for Date, Time, Magnitude, Latitude, Longitude, Depth
  with ListView.Columns.Add do
  begin
    Caption := 'Date';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Time';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Magnitude';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Latitude';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Longitude';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Depth';
    Width := 100;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Intensity';  // New column for Intensity
    Width := 100;
  end;

end;

end.

