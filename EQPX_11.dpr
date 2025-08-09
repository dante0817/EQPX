program EQPX_11;

uses
  Vcl.Forms,
  EQPX_1 in 'EQPX_1.pas' {frmMain},
  UTILS_DB in 'UTILS_DB.pas',
  DM1 in 'DM1.pas' {DataModule1: TDataModule},
  UTILS_fileIO in 'UTILS_fileIO.pas',
  UTILS_settings in 'UTILS_settings.pas',
  UTILS_DateTime in 'UTILS_DateTime.pas',
  UTILS_ATLAS in 'UTILS_ATLAS.pas',
  UTILS_stations in 'UTILS_stations.pas',
  UTILS_Math in 'UTILS_Math.pas',
  UTILS_PhaseData in 'UTILS_PhaseData.pas',
  UTILS_CombineData in 'UTILS_CombineData.pas',
  EQPlotter_data in 'EQPlotter_data.pas',
  UTILS in 'UTILS.pas',
  EQPlot in 'EQPlot.pas',
  UTILS2 in 'UTILS2.pas',
  UTILS_Output in 'UTILS_Output.pas',
  UTILS_Brute_Force in 'UTILS_Brute_Force.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
