unit untlistCompetitions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmListCompetitions = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    ListView1: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmListCompetitions: TfrmListCompetitions;

implementation

{$R *.dfm}

end.
