unit untLogin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,IdURI,
  Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmLogin = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    WebBrowser1: TWebBrowser;
    Memo1: TMemo;
    procedure WebBrowser1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
  private
    { Private declarations }
  public
    { Public declarations }
    var
     LocalSessionToken:string; // gets passed back to the main form
  end;

var
  frmLogin: TfrmLogin;

implementation

{$R *.dfm}

 function VariantToString(AVar: OleVariant): string;
//available from http://delphi.cjcsoft.net/viewthread.php?tid=43936
var
  i: integer;
  V: olevariant;
begin
  Result := '';
  if VarType(AVar) = (varVariant or varByRef) then
     V := Variant(TVarData(AVar).VPointer^)
  else V := AVar;

  if VarType(V) = (varByte or varArray) then
      try
        for i:=VarArrayLowBound(V,1) to VarArrayHighBound(V,1) do
           Result := Result + Chr(Byte(V[i]));
      except;
      end
    else Result := V;
end;

procedure TfrmLogin.WebBrowser1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  PostDataStr:String;
begin
  if (URL = 'https://www.betfair.com/') and (Length(PostData) > 0) then
    begin
      PostDataStr := TIdURI.URLDecode(VariantToString(PostData));
      if pos('SUCCESS',PostDataStr) > 0 then
        begin
          LocalSessionToken :=  copy(PostDataStr,pos('=',PostDataStr)+1,length(PostDataStr));
          LocalSessionToken := copy(LocalSessionToken,1,pos('&',LocalSessionToken)-1);

          webbrowser1.Stop;

          modalresult := mrOk;
        end;
      end;
end;


end.
