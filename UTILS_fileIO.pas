unit UTILS_fileIO;

interface

procedure ListAtlasDataFilesInMemo(const InputDate, InputTime: string);

implementation

uses
  System.SysUtils, Vcl.Dialogs, Winapi.ShellAPI, Winapi.Windows, EQPX_1,
  System.Classes, System.IOUtils;

procedure ListAtlasDataFilesInMemo(const InputDate, InputTime: string);
var
  Year, Month, Day: string;
  FolderPath: string;
  FilesList: TStringList;
  i: Integer;
begin
  with frmMain do
  begin
      // Extract the year, month, and day from the InputDate
      Year := Copy(InputDate, 1, 4);   // '2020'
      Month := Copy(InputDate, 6, 2);  // '01'
      Day := Copy(InputDate, 9, 2);    // '02'

      // Construct the folder path using the base path from lblProgramPath
      FolderPath := IncludeTrailingPathDelimiter(lblProgramPath.Caption) +
                    'DB\ATLASDATA\' + Year + '\' + Month + '\' + Year + Month + Day;

      // Create a TStringList to hold the list of files
      FilesList := TStringList.Create;
      try
        // List all files in the directory
        FilesList.AddStrings(TDirectory.GetFiles(FolderPath, '*.*', TSearchOption.soTopDirectoryOnly));

        // Clear the memo before adding new content
        mmoATLAS_files.Clear;

        // Add the list of files to the memo
        for i := 0 to FilesList.Count - 1 do
        begin
          mmoATLAS_files.Lines.Add(FilesList[i]);
        end;
      finally
        FilesList.Free;
      end;
    end
end;

end.
