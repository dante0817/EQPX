unit UTILS3;

interface

uses
  Classes, StrUtils, Controls, ExtCtrls, EQPX_1, SysUtils, StdCtrls, Grids, System.Types, Dialogs, Forms;

  procedure CreateButtonsInStringGridVertical(StringGrid: TStringGrid; ButtonCount: Integer);

  procedure ButtonClickHandler(Sender: TObject);
  procedure AddButtonsToGrid(Grid: TStringGrid; ButtonCount: Integer; ParentForm: TForm);

  procedure ClearGrid(Grid: TStringGrid);
  function ExtractResult_heading : Boolean;

  procedure SaveGridToFile(StringGrid: TStringGrid; const FilePath: String);
  procedure Save_ErrorResult(LinesToSave: TStrings; const FilePath: String);
  function FindHighestFile(const FilePathPattern: string): string;

type
  TUtils = class
  public
    procedure DuplicateButton(ScrollBox: TScrollBox; OriginalButton: TButton; NumberOfButtons: Integer);
    procedure ButtonClick(Sender: TObject);
  end;

implementation

{ TUtils }

procedure TUtils.ButtonClick(Sender: TObject);
var
  iEQindex : integer;
begin
  iEQindex := StrToInt(TButton(Sender).Caption) - 1;

  if Sender is TButton then
    frmMain.ledEQIndex_2process.Text := IntToStr(iEQindex);

  frmMain.Plot_1st_2Click(nil);

//  frmMain.FinalizeClick(nil);
end;

procedure TUtils.DuplicateButton(ScrollBox: TScrollBox; OriginalButton: TButton; NumberOfButtons: Integer);
var
  i: Integer;
  NewButton: TButton;
  CurrentTop: Integer;
begin
  // Remove all existing buttons from the ScrollBox
  for i := ScrollBox.ControlCount - 1 downto 0 do
  begin
    if ScrollBox.Controls[i] is TButton then
      ScrollBox.Controls[i].Free;
  end;

  CurrentTop := 5; // Start positioning at the top with a small margin

  for i := 1 to NumberOfButtons do
  begin
    // Create a new button
    NewButton := TButton.Create(ScrollBox);
    try
      NewButton.Parent := ScrollBox; // Set parent to the ScrollBox
      NewButton.Caption := IntToStr(i); // Label each button
      NewButton.Width := ScrollBox.ClientWidth - 20; // Fit the button inside the ScrollBox width
      NewButton.Height := OriginalButton.Height; // Use the same height
      NewButton.Left := 5; // Margin from the left
      NewButton.Top := CurrentTop; // Position each button vertically

      NewButton.Align := alTop; // Align buttons at the top of the panel
      NewButton.Margins.Top := 2; // Optional margin for spacing
      NewButton.Margins.Bottom := 2;

      // Assign the ButtonClick event handler
      NewButton.OnClick := ButtonClick;

      // Update the top position for the next button
      CurrentTop := CurrentTop + NewButton.Height + 5; // Add spacing between buttons
    except
      NewButton.Free;
      raise;
    end;
  end;
end;

//-------------------------------
procedure CreateButtonsInStringGridVertical(StringGrid: TStringGrid; ButtonCount: Integer);
var
  i, j, ButtonIndex: Integer;
  Button: TButton;
  CellRect: TRect;
begin
   // Error check: Make sure the StringGrid exists, has columns and rows
    if (StringGrid = nil) or (StringGrid.ColCount <= 0) or (StringGrid.RowCount <= 0) then
    begin
      ShowMessage('StringGrid is not properly set up or doesn''t exist.');
      Exit;
    end;
  ButtonIndex := 0;
    //Loop through the stringgrid to assign buttons
  for i := 0 to StringGrid.ColCount - 1 do // Loop through columns first
  begin
      for j := 0 to StringGrid.RowCount - 1 do
    begin
      if ButtonIndex >= ButtonCount then
          break;
        //Get cell rect
        CellRect := StringGrid.CellRect(i, j);
        //create button dynamically
        Button := TButton.Create(StringGrid);
        //assign the parent as StringGrid
        Button.Parent := StringGrid;
        //Place it into the cell
        Button.Left := CellRect.Left + 1;
        Button.Top := CellRect.Top + 1;
        Button.Width := CellRect.Right - CellRect.Left - 2;
        Button.Height := CellRect.Bottom - CellRect.Top - 2;
        Button.Caption := 'Button ' + IntToStr(ButtonIndex + 1);
        //assign the name
        Button.Name := 'Button' + IntToStr(ButtonIndex+1);
        //incremenet index
        Inc(ButtonIndex);
    end;
    if ButtonIndex >= ButtonCount then
        break;
  end;
end;

procedure ButtonClickHandler(Sender: TObject);
var
  RowNumber: Integer;
begin
  if Sender is TButton then
  begin
    RowNumber := (Sender as TButton).Tag;
    ShowMessage('You clicked the button in row ' + IntToStr(RowNumber));
  end;
end;

procedure AddButtonsToGrid(Grid: TStringGrid; ButtonCount: Integer; ParentForm: TForm);
var
  Row: Integer;
  Button: TButton;
  CellRect: TRect;
begin
  // Loop through each row (excluding fixed header row)
  for Row := Grid.FixedRows to Grid.FixedRows + ButtonCount - 1 do
  begin
    // Get the rectangle for the specific cell
    CellRect := Grid.CellRect(0, Row); // Column 0 (first column)

    // Create the button
    Button := TButton.Create(ParentForm); // Button parent is frmMain
    Button.Parent := ParentForm;         // Set the parent to frmMain
    Button.Caption := IntToStr(Row); // Label the button with its row number
    Button.Tag := Row; // Store the row number in the Tag property

    // Set the button bounds to fit inside the cell
    Button.SetBounds(
      Grid.Left + CellRect.Left,    // Left position
      Grid.Top + CellRect.Top,      // Top position
      CellRect.Width,               // Width
      CellRect.Height               // Height
    );

    // Assign the OnClick event to the button
//    Button.OnClick := ButtonClickHandler;
  end;

  // Force grid repaint to ensure all controls are shown
  Grid.Invalidate;

end;

//-------------------------------
// Helper procedure to clear and reset a grid
procedure ClearGrid(Grid: TStringGrid);
var
  Row, Col: Integer;
begin
  // Clear all cells
  for Row := 0 to Grid.RowCount - 1 do
    for Col := 0 to Grid.ColCount - 1 do
      Grid.Cells[Col, Row] := '';

  // Reset grid to initial state
  Grid.RowCount := 1;
end;

function ExtractResult_heading : Boolean;
begin
  if LeftStr(Trim(frmMain.mmoResult.Lines[2]), 3) = 'SUM' then
    Result := False
  else
    Result := True;
end;

procedure Save_ErrorResult(LinesToSave: TStrings; const FilePath: String);
begin
  // Ensure the directory exists
  if not DirectoryExists(ExtractFilePath(FilePath)) then
  begin
    if not ForceDirectories(ExtractFilePath(FilePath)) then
      raise Exception.Create('Failed to create directory: ' + ExtractFilePath(FilePath));
  end;

  // Save the lines to the file
  try
    LinesToSave.SaveToFile(FilePath);
  except
    on E: Exception do
      raise Exception.Create('Error saving file "' + FilePath + '": ' + E.Message);
  end;
end;

procedure SaveGridToFile(StringGrid: TStringGrid; const FilePath: String);
var
  FileHandle: TextFile;
  Row, Col: Integer;
  Line: String;
begin
  // Write StringGrid content to the file
  AssignFile(FileHandle, FilePath);
  try
    Rewrite(FileHandle);

    // Write the header row
    Line := '';
    for Col := 0 to StringGrid.ColCount - 1 do
    begin
      if Col > 0 then
        Line := Line + ',';
      Line := Line + StringGrid.Cells[Col, 0];
    end;
    Writeln(FileHandle, Line);

    // Write each row
    for Row := 1 to StringGrid.RowCount - 1 do
    begin
      Line := ''; // Initialize the line before adding data
      for Col := 0 to StringGrid.ColCount - 1 do
      begin
        if Col > 0 then
          Line := Line + ',';
        Line := Line + StringGrid.Cells[Col, Row];
      end;
      Writeln(FileHandle, Line);
    end;

  finally
    CloseFile(FileHandle); // Ensure the file is properly closed
  end;
end;

function FindHighestFile(const FilePathPattern: string): string;
var
  SearchRec: TSearchRec;
  FoundFiles: TStringList;
  HighestValue, CurrentValue: Integer;
  sHighestFile, FileNameWithoutExt, FileNumberStr: string;
begin
  Result := '';
  HighestValue := -1;
  sHighestFile := '';

  // Initialize the list to store found files
  FoundFiles := TStringList.Create;
  try
    // Search for matching files
    if FindFirst(FilePathPattern, faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          // Add the full file path to the list
          FoundFiles.Add(ExtractFilePath(FilePathPattern) + SearchRec.Name);
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;

    // If no files were found, exit
    if FoundFiles.Count = 0 then
      Exit;

    // Determine the file with the highest value
    for var i := 0 to FoundFiles.Count - 1 do
    begin
      // Extract the file name without path and extension
      FileNameWithoutExt := ChangeFileExt(ExtractFileName(FoundFiles[i]), '');

      // Extract the part after the last underscore
      FileNumberStr := RightStr(FileNameWithoutExt, 1);
      if (RightStr(FileNameWithoutExt, 1)) = 'F' then
      begin
        FileNumberStr := RightStr(FileNameWithoutExt, 2);
        FileNumberStr := LeftStr(FileNumberStr, 1);
      end;

      // Try to convert to an integer
      if TryStrToInt(FileNumberStr, CurrentValue) then
      begin
        if CurrentValue > HighestValue then
        begin
          HighestValue := CurrentValue;
          sHighestFile := FoundFiles[i];
        end;
      end;
    end;

    // Return the highest file found
    Result := sHighestFile;

  finally
    FoundFiles.Free;
  end;
end;

end.

