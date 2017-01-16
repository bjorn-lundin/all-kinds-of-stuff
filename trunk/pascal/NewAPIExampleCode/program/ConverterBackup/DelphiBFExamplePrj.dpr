program DelphiBFExamplePrj;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {frmMain},
  untLogin in 'untLogin.pas' {frmLogin},
  untlistCompetitions in 'untlistCompetitions.pas' {frmListCompetitions},
  untListCountries in 'untListCountries.pas' {frmlistCountries},
  untListEvents in 'untListEvents.pas' {frmListEvents};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmListEvents, frmListEvents);
  Application.Run;
end.
