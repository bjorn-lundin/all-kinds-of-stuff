unit untListCountries;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmlistCountries = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    ListView1: TListView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmlistCountries: TfrmlistCountries;

implementation

{$R *.dfm}

end.
