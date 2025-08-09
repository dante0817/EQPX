unit UTILS_Brute_Force;

interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Math,
  Vcl.Grids, Vcl.Dialogs;

type
  TPhaseInfo = record
    Row: Integer;
    MarkerCol: Integer;  // 3 for P, 6 for S in sgMainData
    IsP: Boolean;
  end;

  TComboInfo = record
    Mask      : Cardinal;
    RMS       : Double;
    UsedP     : Integer;
    UsedS     : Integer;
    Residuals : TArray<Double>;
  end;

var
  PhaseList: TArray<TPhaseInfo>;

procedure BuildPhaseList;
function  MaskIsValid(mask: Cardinal): Boolean;
procedure ApplyMask(mask: Cardinal);
function  ResidualsAreValid(mask: Cardinal; out ResOut: TArray<Double>): Boolean;
procedure BruteForceSearch;

implementation

uses
  EQPX_1;

{-------------------------------------------------------------------------------
  BuildPhaseList:
-------------------------------------------------------------------------------}
procedure BuildPhaseList;
var
  r, idx: Integer;
  pi: TPhaseInfo;
begin
  SetLength(PhaseList, 0);
  with frmMain.sgMainData do
    for r := 1 to RowCount - 1 do
    begin
      if Trim(Cells[1, r]) = '' then
        Break;  // no more entries

      // P-pick
      if Cells[3, r] <> '*' then
      begin
        pi.Row       := r;
        pi.MarkerCol := 3;
        pi.IsP       := True;
        idx := Length(PhaseList);
        SetLength(PhaseList, idx + 1);
        PhaseList[idx] := pi;
      end;

      // S-pick (only if P exists and no '*' in col6)
      if (Trim(Cells[4, r]) <> '') and (Cells[6, r] <> '*') then
      begin
        pi.Row       := r;
        pi.MarkerCol := 6;
        pi.IsP       := False;
        idx := Length(PhaseList);
        SetLength(PhaseList, idx + 1);
        PhaseList[idx] := pi;
      end;
    end;
end;

{-------------------------------------------------------------------------------
  MaskIsValid:
-------------------------------------------------------------------------------}
function MaskIsValid(mask: Cardinal): Boolean;
var
  i, cntP, cntS, j: Integer;
begin
  cntP := 0; cntS := 0;

  // count P and S bits
  for i := 0 to High(PhaseList) do
    if (mask and (1 shl i)) <> 0 then
      if PhaseList[i].IsP then
        Inc(cntP)
      else
        Inc(cntS);

  // enforce S-requires-P
  for i := 0 to High(PhaseList) do
    if (not PhaseList[i].IsP) and ((mask and (1 shl i)) <> 0) then
    begin
      // find matching P in same row
      j := 0;
      while (j <= High(PhaseList)) and not (
        PhaseList[j].IsP and
        (PhaseList[j].Row = PhaseList[i].Row)
      ) do
        Inc(j);

      if (j > High(PhaseList)) or ((mask and (1 shl j)) = 0) then
        Exit(False);
    end;

  // enforce minimum counts
  Result :=
    (cntP >= StrToIntDef(frmMain.ledMinimum_P.Text, 3)) and
    (cntS >= StrToIntDef(frmMain.ledMinimum_S.Text, 1));
end;

{-------------------------------------------------------------------------------
  ApplyMask:
-------------------------------------------------------------------------------}
procedure ApplyMask(mask: Cardinal);
var
  i: Integer;
begin
  with frmMain.sgMainData do
  begin
    // clear old markers
    for i := 1 to RowCount - 1 do
    begin
      Cells[3, i] := '';
      Cells[6, i] := '';
    end;

    // mark excluded picks
    for i := 0 to High(PhaseList) do
      if (mask and (1 shl i)) = 0 then
        Cells[PhaseList[i].MarkerCol, PhaseList[i].Row] := '*';
  end;
end;

{-------------------------------------------------------------------------------
  ResidualsAreValid:
-------------------------------------------------------------------------------}
function ResidualsAreValid(mask: Cardinal; out ResOut: TArray<Double>): Boolean;
var
  i: Integer;
  resVal: Double;
  tmp: TArray<Double>;
begin
  SetLength(tmp, 0);
  with frmMain.sgMainData do
    for i := 0 to High(PhaseList) do
      if (mask and (1 shl i)) <> 0 then
      begin
        if PhaseList[i].IsP then
          resVal := StrToFloatDef(Cells[2, PhaseList[i].Row], 1e9)
        else
          resVal := StrToFloatDef(Cells[5, PhaseList[i].Row], 1e9);

        if Abs(resVal) >= 1 then
          Exit(False);

        SetLength(tmp, Length(tmp) + 1);
        tmp[High(tmp)] := resVal;
      end;

  ResOut := tmp;
  Result := True;
end;

{-------------------------------------------------------------------------------
  BruteForceSearch:
-------------------------------------------------------------------------------}
procedure BruteForceSearch;
var
  mask, maxMask: Cardinal;
  ComboList: TArray<TComboInfo>;
  ci: TComboInfo;
  bestIdx, i: Integer;
  sw: TStopwatch;
  elapsed: Double;
  sel: TStringList;
  station: string;
begin
  // initial data & plot
  frmMain.EQP_dataClick(nil);
  frmMain.PlotClick(nil);
  frmMain.RePlotClick(nil);

  sw := TStopwatch.StartNew;
  BuildPhaseList;
  if Length(PhaseList) = 0 then
    raise Exception.Create('No valid P/S picks found');

  maxMask := (1 shl Length(PhaseList)) - 1;
  SetLength(ComboList, 0);

  // iterate all masks
  for mask := 1 to maxMask do
    if MaskIsValid(mask) then
    begin
      ApplyMask(mask);

      // run solver/plot sequence
      frmMain.PLOT_DATClick(nil);
      frmMain.PHILSTA_DATClick(nil);
      frmMain.PLOT_eqClick(nil);
      frmMain.ExtractResult_headingClick(nil);
      frmMain.Update_res_PSClick(nil);
      frmMain.Used_P_SClick(nil);

      ci.RMS := StrToFloatDef(frmMain.ledRMS.Text, 1e9);

      if (ci.RMS < 1) and ResidualsAreValid(mask, ci.Residuals) then
      begin
        ci.Mask  := mask;
        ci.UsedP := 0;
        ci.UsedS := 0;
        for i := 0 to High(PhaseList) do
          if (mask and (1 shl i)) <> 0 then
            if PhaseList[i].IsP then Inc(ci.UsedP)
            else Inc(ci.UsedS);

        i := Length(ComboList);
        SetLength(ComboList, i + 1);
        ComboList[i] := ci;
      end;
    end;

  // pick best combo
  if Length(ComboList) = 0 then
    raise Exception.Create('No combination satisfied RMS < 1 and residual < 1');

  bestIdx := 0;
  for i := 1 to High(ComboList) do
    if (ComboList[i].UsedP + ComboList[i].UsedS >
        ComboList[bestIdx].UsedP + ComboList[bestIdx].UsedS)
    or ((ComboList[i].UsedP + ComboList[i].UsedS =
         ComboList[bestIdx].UsedP + ComboList[bestIdx].UsedS)
        and (ComboList[i].RMS < ComboList[bestIdx].RMS)) then
      bestIdx := i;

  // re-apply best mask
  ApplyMask(ComboList[bestIdx].Mask);
  frmMain.PLOT_DATClick(nil);
  frmMain.PHILSTA_DATClick(nil);
  frmMain.PLOT_eqClick(nil);
  frmMain.ExtractResult_headingClick(nil);
  frmMain.Update_res_PSClick(nil);
  frmMain.Used_P_SClick(nil);

  // display which stations and phases were chosen
  sel := TStringList.Create;
  try
    for i := 0 to High(PhaseList) do
      if (ComboList[bestIdx].Mask and (1 shl i)) <> 0 then
      begin
        station := frmMain.sgMainData.Cells[0, PhaseList[i].Row];
        if PhaseList[i].IsP then
          sel.Add(Format('%s: P', [station]))
        else
          sel.Add(Format('%s: S', [station]));
      end;
    ShowMessage('Best combination picks:' + sLineBreak + sel.Text);
  finally
    sel.Free;
  end;

  sw.Stop;
  elapsed := sw.ElapsedMilliseconds / 1000;

  ShowMessage(Format(
    'Brute-force complete:'#13#10 +
    '  Kept %d combinations (RMS < 1, residuals < 1)'#13#10 +
    '  BEST uses %d P + %d S (mask=%u)'#13#10 +
    '  Best RMS = %.4f'#13#10 +
    '  Elapsed time = %.2f s',
    [ Length(ComboList),
      ComboList[bestIdx].UsedP,
      ComboList[bestIdx].UsedS,
      ComboList[bestIdx].Mask,
      ComboList[bestIdx].RMS,
      elapsed ]
  ));
end;

end.


