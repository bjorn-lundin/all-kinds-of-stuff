unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,idHttp,IdSSLOpenSSL, Vcl.ComCtrls,
  untLogin,
  untListCountries,
  untlistCompetitions,
  untListEvents;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    GroupBox1: TGroupBox;
    leAppKey: TLabeledEdit;
    LabeledEdit1: TLabeledEdit;
    leDelayedAppKey: TLabeledEdit;
    Button1: TButton;
    Panel4: TPanel;
    GroupBox2: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    GroupBox3: TGroupBox;
    btnLogin: TButton;
    Memo1: TMemo;
    rgServer: TRadioGroup;
    GroupBox4: TGroupBox;
    leMemBefore: TLabeledEdit;
    leMemAfter: TLabeledEdit;
    Button9: TButton;
    Button10: TButton;
   // procedure Button2Click(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
    Function  listCompetitions:string;
    function  listCountries:string;
    function  showUserDetails:string;
    Function  GetEvents:string;
    function  showAccount:string;
    function  listTimeRanges:string;
    function  listvenues:string;

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  SessionToken  : String;
  App_Key       : String;
  DelayedAppKey : String;
  loggedIn      : boolean;

implementation

{$R *.dfm}

function MemUsed: cardinal;
var
    st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
begin
    GetMemoryManagerState(st);
    result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
    for sb in st.SmallBlockTypeStates do begin
        result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
    end;
end;

function StreamToString(aStream: TStream): string;
//code from http://stackoverflow.com/questions/301991/what-s-the-simplest-way-to-call-http-post-url-using-delphi
var
  SS: TStringStream;
begin
  if aStream <> nil then
  begin
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(aStream, 0);  // No need to position at 0 nor provide size
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  end else
  begin
    Result := '';
  end;
end;


function TfrmMain.showAccount:string;
const
 Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/getAccountFunds/';
 UnitedKindom = 'https://api.betfair.com/exchange/account/rest/v1.0/getAccountFunds/';
var
  httpClient   : TIdHttp ;
  sslIOHandler : TIdSSLIOHandlerSocketOpenSSL ;
  Response     : string ;
  SourseURL    : string;
  paramstr     : string;
  Res1,Params  : Tstringstream;
 begin
 Response := '' ;
 paramstr := '{"filter":{},"id":"1","jsonrpc":"2.0"}';
 Params := TStringStream.Create();
 Params.WriteString(paramstr);
 Res1 :=  TStringStream.Create();

try
  try
   httpClient := TIdHttp.Create(nil) ;
   httpClient.Request.ContentType := 'application/json;';
   httpClient.Request.Charset := 'utf-8';
   httpClient.Request.ContentLength := sizeof(Params);
   httpClient.Request.Connection    := 'Keep-Alive';
   httpClient.Request.Accept := 'application/json' ;
   httpClient.ReadTimeout    := 60000 ;
   httpClient.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   httpClient.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIOHandler         := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   httpClient.IOHandler := sslIOHandler ;

    case rgServer.ItemIndex of
     0: sourseURL := Australian;
     1: sourseURL := UnitedKindom;
    end;

  // NOTE:  The Australian getAccountFunds will be in a future BetFair API-NG release

  //therefore
  sourseURL := UnitedKindom;

   httpClient.Post(sourseurl,Params,res1);
   except
      on E: Exception do
        begin
         Response := E.Message; //HTTP/1.1 400 Bad Request  //'HTTP/1.1 404 Not Found'
        end;
   end;
   Response := StreamToString(res1);
   result := Response;
 finally
  httpClient:= nil;
  httpClient.Free;
  sslIOHandler:= nil;
  sslIOHandler.Free;
  Res1 := nil;
  Res1.Free;
  Params := nil;
  params.Free;
 end;
end;


procedure TfrmMain.Button10Click(Sender: TObject);
var
 response:string;
 availableToBetBalance,exposure,retainedCommission,exposureLimit,discountRate,pointsBalance,wallet:string;

 data:tstringlist;
begin
  leMemBefore.Text := inttostr(memused);
  data:=tstringlist.create;
  response := ShowAccount;

  availableToBetBalance := copy(Response,pos('"availableToBetBalance":',response)+24,length(response));
  availableToBetBalance := copy(availableToBetBalance,1,pos(',',availableToBetBalance)-1);
  availableToBetBalance := stringreplace(availableToBetBalance,'"','',[rfReplaceAll]);

  exposure := copy(Response,pos('"exposure":',response)+11,length(response));
  exposure := copy(exposure,1,pos(',',exposure)-1);
  exposure := stringreplace(exposure,'"','',[rfReplaceAll]);

  retainedCommission := copy(Response,pos('"retainedCommission":',response)+21,length(response));
  retainedCommission := copy(retainedCommission,1,pos(',',retainedCommission)-1);
  retainedCommission:= stringreplace(retainedCommission,'"','',[rfReplaceAll]);

  exposureLimit := copy(Response,pos('"exposureLimit":',response)+16,length(response));
  exposureLimit := copy(exposureLimit,1,pos(',',exposureLimit)-1);
  exposureLimit:= stringreplace(exposureLimit,'"','',[rfReplaceAll]);

  discountRate := copy(Response,pos('"discountRate":',response)+15,length(response));
  discountRate := copy(discountRate,1,pos(',',discountRate)-1);
  discountRate:= stringreplace(discountRate,'"','',[rfReplaceAll]);

  pointsBalance := copy(Response,pos('"pointsBalance":',response)+16,length(response));
  pointsBalance := copy(pointsBalance,1,pos(',',pointsBalance)-1);
  pointsBalance:= stringreplace(pointsBalance,'"','',[rfReplaceAll]);


  wallet := copy(Response,pos('"wallet":"',response)+10,length(response));
  wallet := copy(wallet,1,pos('}',wallet)-1);
  wallet:= stringreplace(wallet,'"','',[rfReplaceAll]);

  data.Add('AvailableToBetBalance :'+availableToBetBalance);
  data.Add('RetainedCommission    :'+retainedCommission);
  data.Add('ExposureLimit         :'+exposureLimit);
  data.Add('DiscountRate          :'+discountRate);
  data.Add('PointsBalance         :'+pointsBalance);
  data.Add('Wallet                :'+wallet);

  showmessage(data.Text);

  freeandnil(data);
  leMemAfter.Text := inttostr(memused);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  slAppKeys:Tstringlist;
  Path,Temp:string;
  filename:string;
begin
 // save the app keys in a txt file for later recovery...
 // Normally these would be constants in your program

  if (trim(leAppkey.Text) = '') or  (trim(leDelayedAppkey.Text) ='') then
   exit;  // if nothing is entered then don't save the file

  Path := extractfilepath(application.ExeName);
  filename := 'AppKeys.txt';
  slAppKeys:=Tstringlist.create;
  slAppKeys.Add('<APP_KEY>'+trim(leAppkey.Text));
  slAppKeys.Add('<DELAYED_APP_KEY>'+trim(leDelayedAppkey.Text));
  slAppKeys.SaveToFile(path+filename);

  freeandnil(slAppKeys);
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
const
  loginUrl ='https://identitysso.betfair.com/view/login?product=';
var
 frmLogin:TfrmLogin;

begin
  leMembefore.Text := inttostr(memused);
  frmLogin:=TfrmLogin.create(frmMain);
  frmLogin.WebBrowser1.Navigate(loginUrl);
  frmLogin.ShowModal;

  if frmLogin.ModalResult = mrOK then
   begin
     loggedIn := true;  // logout is in the Form Close
     SessionToken :=  frmLogin.LocalSessionToken; // passed back from the login form and used in all requests
     GroupBox2.Enabled := true;  // enable button panel
   end;
  freeandnil(frmLogin);
  leMemafter.Text := inttostr(memused);
end;

Function  TfrmMain.listCompetitions:string;
 const
 Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/listCompetitions/';
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listCompetitions/';

var
  http         : TIdHttp ;
  sslIO        : TIdSSLIOHandlerSocketOpenSSL ;

  Filter       : string;
  ResultData   :Tstringlist;  // the response will be put in a stringlist;
  ReturnStr,Params :Tstringstream;
  answer,TEMP:string;
  paramstr:string;
  sourseURL:string;

begin


   ResultData   :=Tstringlist.create;
   Params    := TStringStream.Create();
   paramstr  := '{"filter":{},"id":"1","jsonrpc":"2.0"}';      // we'll get all.. this can be used to filter specific competitions..
   Params.WriteString(paramstr);
   ReturnStr :=  TStringStream.Create();
try
 try
   http := TIdHttp.Create(nil) ;
   http.Request.ContentType := 'application/json;';
   http.Request.Charset := 'utf-8';
   http.Request.ContentLength := sizeof(Params);
   http.Request.Connection    := 'Keep-Alive';
   http.Request.Accept := 'application/json' ;
   http.ReadTimeout    := 60000 ;
   http.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   http.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIO       := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   http.IOHandler := sslIO ;

   case rgServer.ItemIndex of
     0: sourseURL := Australian;
     1: sourseURL := UnitedKindom;
   end;


   http.Post(sourseurl,Params,ReturnStr);  // send the params to the url and put the response in ReturnStr
  except
      on E: Exception do
        begin
         ResultData.Add(E.Message); //HTTP/1.1 400 Bad Request  means the param sting is probably wrong
        end;
   end;
  ResultData.text := StreamToString(ReturnStr); // convert a byte string into a  text string

 finally
    result := ResultData.text;
    ReturnStr.Free;
    params.Free;
    http := nil;
    http.Free;
    sslIO := nil;
    sslIO.Free;
    freeandnil(ResultData);

 end;
end;


procedure TfrmMain.Button3Click(Sender: TObject);
// example line
// {"competition":{"id":"6192749","name":"New Zealand v South Africa 2014"},"marketCount":24,"competitionRegion":"NZL"},
// {"competition":{"id":"892425","name":"Czech 3. Liga"},"marketCount":2,"competitionRegion":"CZE"},
var
 data:tstringlist;
 i:integer;
 temp:string;
 id,name,marketCount,competitionRegion:string;
 listitem:Tlistitem;
 frmlistCompetitions:TfrmlistCompetitions;
begin
 leMembefore.Text := inttostr(memused);

  data:=tstringlist.create;

  data.Text := listCompetitions; // returns the raw data from listCompetitions

  { While the following code is not elegant,
    it works and without Memory Leaks.

    I tried many functions to convert the JSON pairs into arrays
    and all of the methods I tried either didn’t work or
    produced memory leaks, so this code is suggested as an quick and nasty alternative
    but feel free to decode these strings anyway you wish.
   }

  frmlistCompetitions:=TfrmlistCompetitions.create(frmMain);
  frmlistCompetitions.ListView1.clear;

  // first split resulting text into competition lines  by adding a carriage return+line feed at the seperator
  data.Text := stringreplace(data.Text,'},{','},'+#13#10+'{',[rfreplaceall]);
 for I := 0 to data.Count -1 do
   begin
     temp := data.Strings[i];
     id := copy(temp,pos('"id":"',temp)+6,length(temp));
     id := copy(id,1,pos(',',id)-1);
     id := stringreplace(id,'"','',[rfreplaceall]);
     id := stringreplace(id,'}','',[rfreplaceall]);

     name := copy(temp,pos('"name":"',temp)+8,length(temp));
     name := copy(name,1,pos(',',name)-1);
     name := stringreplace(name,'"','',[rfreplaceall]);
     name := stringreplace(name,'}','',[rfreplaceall]);

     marketCount := copy(temp,pos('"marketCount":',temp)+14,length(temp));
     marketCount := copy(marketCount,1,pos(',',marketCount)-1);
     marketCount := stringreplace(marketCount,'"','',[rfreplaceall]);
     marketCount := stringreplace(marketCount,'}','',[rfreplaceall]);

     competitionRegion := copy(temp,pos('"competitionRegion":"',temp)+21,length(temp));
     competitionRegion := copy(competitionRegion,1,pos(',',competitionRegion)-1);
     competitionRegion := stringreplace(competitionRegion,'"','',[rfreplaceall]);
     competitionRegion := stringreplace(competitionRegion,'}','',[rfreplaceall]);

     listitem := frmlistCompetitions.ListView1.Items.Add();
     listitem.Caption := id;
     listitem.SubItems.Add(name);
     listitem.SubItems.Add(marketCount);
     listitem.SubItems.Add(competitionRegion);
   end;

  frmlistCompetitions.ShowModal;


  frmlistCompetitions := nil;
  frmlistCompetitions.Free;

//  freeandnil(frmlistCompetitions);
  freeandnil(data);
 leMemAfter.Text := inttostr(memused);
end;

function TfrmMain.listCountries:string;
 const
// n/a Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/listCompetitions/';
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listCountries/';

var
  http         : TIdHttp ;
  sslIO        : TIdSSLIOHandlerSocketOpenSSL ;

  ResultData   :Tstringlist;  // the response will be put in a stringlist;
  ReturnStr,Params :Tstringstream;

  paramstr:string;
  sourseURL:string;

begin
   ResultData   :=Tstringlist.create;
   Params := TStringStream.Create();
   paramstr:= '{"filter":{},"id":"1","jsonrpc":"2.0"}';      // we'll get all..
   Params.WriteString(paramstr);
   ReturnStr :=  TStringStream.Create();
try
 try
   http := TIdHttp.Create(nil) ;
   http.Request.ContentType := 'application/json;';
   http.Request.Charset := 'utf-8';
   http.Request.ContentLength := sizeof(Params);
   http.Request.Connection    := 'Keep-Alive';
   http.Request.Accept := 'application/json' ;
   http.ReadTimeout    := 60000 ;
   http.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   http.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIO       := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   http.IOHandler := sslIO ;

   sourseURL := UnitedKindom;

   http.Post(sourseurl,Params,ReturnStr);  // send the params to the url and put the response in ReturnStr
   except
      on E: Exception do
        begin
         ResultData.Add(E.Message); //HTTP/1.1 400 Bad Request  means the param sting is probably wrong
        end;
   end;
   ResultData.text := StreamToString(ReturnStr); // convert a byte string into a  text string


 finally
    ReturnStr.Free;
    params.Free;
    result := ResultData.text;
    http := nil;
    http.Free;
    sslIO := nil;
    sslIO.Free;

   freeandnil(ResultData);
 end;
end;


procedure TfrmMain.Button4Click(Sender: TObject);
//'[{"countryCode":"GB","marketCount":2351},'
var
 data:tstringlist;
 frmListCountries:TfrmListCountries;
 Temp,countryCode,marketCount:string;
 i:integer;
 listitem:Tlistitem;
begin
 leMembefore.Text := inttostr(memused);
 data:=tstringlist.create;
 frmlistCountries := TfrmlistCountries.Create(frmMain);
 frmlistCountries.ListView1.Clear;
 data.Text := listCountries;
 // first split resulting text into country lines  by adding a carriage return+line feed at the seperator
 data.Text := stringreplace(data.Text,'},{','},'+#13#10+'{',[rfreplaceall]);
 for i := 0 to data.Count -1 do
  begin
     temp := data.strings[i];
     countryCode := copy(temp,pos('"countryCode":',temp)+14,length(temp));
     countryCode := copy(countryCode,1,pos(',',countryCode)-1);
     countryCode := stringreplace(countryCode,'"','',[rfreplaceall]);
     countryCode := stringreplace(countryCode,'}','',[rfreplaceall]);

     marketCount := copy(temp,pos('"marketCount":',temp)+14,length(temp));
     marketCount := copy(marketCount,1,pos(',',marketCount)-1);
     marketCount := stringreplace(marketCount,'"','',[rfreplaceall]);
     marketCount := stringreplace(marketCount,'}','',[rfreplaceall]);

     listitem:=frmListCountries.ListView1.Items.Add();

     listitem.Caption := countryCode;
     listitem.SubItems.Add(marketCount);
  end;

  frmListCountries.ShowModal;


 freeandnil(frmListCountries);
 freeandnil(data);
 leMemAfter.Text := inttostr(memused);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
var
 data,data2:Tstringlist;
 listitem:tlistitem;
 frmListEvents:TfrmListEvents;
 i:integer;
begin
  leMembefore.Text := inttostr(memused);
  data:=Tstringlist.create;
  data2:=Tstringlist.create;
  data.Text := getEvents;
  frmListEvents:=TfrmListEvents.Create(frmMain);
  frmListEvents.ListView1.Clear;
  for i := 0 to data.Count-1 do
   begin
     data2.CommaText := data.Strings[i];
     listitem := frmListEvents.ListView1.Items.add();
     listitem.Caption := data2.Values['EventID'];
     listitem.SubItems.Add(data2.Values['Venue']);
     listitem.SubItems.Add(data2.Values['CountryCode']);
     listitem.SubItems.Add(data2.Values['OpenDate']);
     listitem.SubItems.Add(data2.Values['TimeZone']);
   end;

  frmListEvents.ShowModal;

  freeandnil(frmListEvents);
  freeandnil(data2);
  freeandnil(data);
  leMemAfter.Text := inttostr(memused);
end;

function listMarketTypes:string;

 const
// n/a Australian
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listMarketTypes/';

var
  http         : TIdHttp ;
  sslIO        : TIdSSLIOHandlerSocketOpenSSL ;

  ResultData   :Tstringlist;  // the response will be put in a stringlist;
  ReturnStr,Params :Tstringstream;

  paramstr:string;
  sourseURL:string;

begin
   ResultData   :=Tstringlist.create;
   Params := TStringStream.Create();
   paramstr:= '{"filter":{},"id":"1","jsonrpc":"2.0"}';      // we'll get all..
   Params.WriteString(paramstr);
   ReturnStr :=  TStringStream.Create();
try
 try
   http := TIdHttp.Create(nil) ;
   http.Request.ContentType := 'application/json;';
   http.Request.Charset := 'utf-8';
   http.Request.ContentLength := sizeof(Params);
   http.Request.Connection    := 'Keep-Alive';
   http.Request.Accept := 'application/json' ;
   http.ReadTimeout    := 60000 ;
   http.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   http.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIO       := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   http.IOHandler := sslIO ;

   sourseURL := UnitedKindom;

   http.Post(sourseurl,Params,ReturnStr);  // send the params to the url and put the response in ReturnStr
   except
      on E: Exception do
        begin
         ResultData.Add(E.Message); //HTTP/1.1 400 Bad Request  means the param sting is probably wrong
        end;
   end;
   ResultData.text := StreamToString(ReturnStr); // convert a byte string into a  text string


 finally
    ReturnStr.Free;
    params.Free;
    result := ResultData.text;
    http := nil;
    http.Free;
    sslIO := nil;
    sslIO.Free;

   freeandnil(ResultData);
 end;
end;



procedure TfrmMain.Button6Click(Sender: TObject);
var
 i:integer;
 data:tstringlist;
 slResponse:Tstringlist;

 Temp,marketType,marketCount:string;
// {"marketType":"NONSPORT","marketCount":143},{"
begin
 leMembefore.Text := inttostr(memused);
 data:=tstringlist.create;
 slResponse:=Tstringlist.Create;
  data.Text := listMarketTypes;
  data.Text := stringreplace(data.Text,'},{','},'+#13#10+'{',[rfreplaceall]);
  for i := 0 to 19 do // limit it to 20 to make the display readable but you can recover all if needed
   begin
      temp := data.Strings[i];
      marketType := copy(temp,pos('"marketType":"',temp)+14,length(temp));
      marketType := copy(marketType,1,pos(',',marketType)-1);
      marketType := stringreplace(marketType,'"','',[rfreplaceall]);

      marketCount := copy(temp,pos('"marketCount":',temp)+14,length(temp));
      marketCount := copy(marketCount,1,pos('}',marketType)-1);
      marketCount := stringreplace(marketCount,'"','',[rfreplaceall]);
      marketCount := stringreplace(marketCount,'}','',[rfreplaceall]);
      slResponse.Add('MarketType :'+marketType +' MarketCount'+marketCount);
   end;
   slResponse.Add('');
  slResponse.Add('There are '+inttostr(data.count)+' responses, 20 are shown');

  showmessage(slResponse.Text);

 freeandnil(data);
 freeandnil(slResponse);
 leMemAfter.Text := inttostr(memused);
end;

function  TfrmMain.listTimeRanges:string;
const
 Australian   = 'https://api.betfair.com/exchange/betting/rest/v1.0/listTimeRanges/';
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listTimeRanges/';

var
  http         : TIdHttp ;
  sslIO        : TIdSSLIOHandlerSocketOpenSSL ;

  ResultData   :Tstringlist;  // the response will be put in a stringlist;
  ReturnStr,Params :Tstringstream;

  paramstr:string;
  sourseURL:string;

begin
   ResultData   :=Tstringlist.create;
   Params := TStringStream.Create();
   // this is how I understand Granuality.. (I can be wrong, wont be the first time
   // "DAYS" gives Days without Hours & minutes
   //"HOURS" gives day & hours with minutes
   //"MINUTES" gives days,hours & minutes..


   paramstr:= '{"filter":{"eventTypeIds":["7"]},"granularity":"MINUTES"},"id":"1","jsonrpc":"2.0"}';  // 7 is Horse Racing
   Params.WriteString(paramstr);
   ReturnStr :=  TStringStream.Create();
try
 try
   http := TIdHttp.Create(nil) ;
   http.Request.ContentType := 'application/json;';
   http.Request.Charset := 'utf-8';
   http.Request.ContentLength := sizeof(Params);
   http.Request.Connection    := 'Keep-Alive';
   http.Request.Accept := 'application/json' ;
   http.ReadTimeout    := 60000 ;
   http.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   http.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIO       := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   http.IOHandler := sslIO ;

  case rgServer.ItemIndex of
     0: sourseURL := Australian;
     1: sourseURL := UnitedKindom;
   end;

   http.Post(sourseurl,Params,ReturnStr);  // send the params to the url and put the response in ReturnStr
   except
      on E: Exception do
        begin
         ResultData.Add(E.Message); //HTTP/1.1 400 Bad Request  means the param sting is probably wrong
        end;
   end;
   ResultData.text := StreamToString(ReturnStr); // convert a byte string into a  text string


 finally
    ReturnStr.Free;
    params.Free;
    result := ResultData.text;
    http := nil;
    http.Free;
    sslIO := nil;
    sslIO.Free;

   freeandnil(ResultData);
 end;
end;


procedure TfrmMain.Button7Click(Sender: TObject);
var
 data,slResult:Tstringlist;
 i:integer;
 temp,timeRange,timeFrom,TimeTo,marketCount:string;
begin
//'[{"timeRange":{"from":"2013-10-10T00:00:00.000Z","to":"2013-10-11T00:00:00.000Z"},"marketCount":1},{"timeRange":{"from":"2014-01-01T00:00:00.000Z","to":"2014-01-02T00:00:00.000Z"},"marketCount":1},{"timeRange":{"from":"2014-03-29T00:00:00.000Z","to":"2014-03-30T00:00:00.000Z"},"marketCount":1},{"timeRange":{"from":"2014-10-27T00:00:00.000Z","to":"2014-10-28T00:00:00.000Z"},"marketCount":156},{"timeRange":{"from":"2014-10-28T00:00:00.000Z","to":"2014-10-29T00:00:00.000Z"},"marketCount":10},

 leMembefore.Text := inttostr(memused);
 slResult:=Tstringlist.create;
 data:=Tstringlist.create;
 data.Text := listTimeRanges;
 data.Text := stringreplace(data.Text,'},{','},'+#13#10+'{',[rfreplaceall]);
 for i := 0 to 19 do   // just show 20
  begin
    temp :=  data.Strings[i];
    timeRange := copy(temp,pos('"timeRange":',temp)+12,length(temp));
    timeRange := copy(timeRange,1,pos('},',timeRange)); // keep the "}' as the end marker
      timeFrom := copy(timeRange,pos('"from":"',timeRange)+8,length(timerange));
      timeFrom := copy(timeFrom,1,pos(',',timefrom)-1);
      timeFrom := stringreplace(timeFrom,'"','',[rfreplaceall]);

      timeTo := copy(timeRange,pos('"to":"',timeRange)+6,length(timerange));
      timeTo := copy(timeTo,1,pos('}',timeTo)-1);
      timeTo := stringreplace(timeTo,'"','',[rfreplaceall]);

    marketCount := copy(temp,pos('"marketCount":',temp)+14,length(temp));
    marketCount := copy(marketCount,1,pos('},',marketCount)-1);
    marketCount := stringreplace(marketCount,'"','',[rfreplaceall]);
    slResult.Add('TimeFrom :'+timeFrom+'  TimeTo :'+timeto+'  Marketcount :'+marketcount);
  end;

 showmessage(slResult.Text);

 freeandnil(slResult);
 freeandnil(data);
 leMemAfter.Text := inttostr(memused);
end;

function TfrmMain.listvenues:string;
const
 Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/listVenues/';
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listVenues/';
var
  httpClient   : TIdHttp ;
  sslIOHandler : TIdSSLIOHandlerSocketOpenSSL ;
  Response     : string ;
  SourseURL    : string;
  paramstr     : string;
  Res1,Params  : Tstringstream;
 begin
 Response := '' ;
 paramstr := '{"filter":{"eventTypeIds":["7"]},"id":"1","jsonrpc":"2.0"}';
 Params := TStringStream.Create();
 Params.WriteString(paramstr);
 Res1 :=  TStringStream.Create();

try
  try
   httpClient := TIdHttp.Create(nil) ;
   httpClient.Request.ContentType := 'application/json;';
   httpClient.Request.Charset := 'utf-8';
   httpClient.Request.ContentLength := sizeof(Params);
   httpClient.Request.Connection    := 'Keep-Alive';
   httpClient.Request.Accept := 'application/json' ;
   httpClient.ReadTimeout    := 60000 ;
   httpClient.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   httpClient.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIOHandler         := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   httpClient.IOHandler := sslIOHandler ;

  case rgServer.ItemIndex of
     0: sourseURL := Australian;
     1: sourseURL := UnitedKindom;
   end;


   httpClient.Post(sourseurl,Params,res1);
   except
      on E: Exception do
        begin
         Response := E.Message; //HTTP/1.1 400 Bad Request  //'HTTP/1.1 404 Not Found'
        end;
   end;
   Response := StreamToString(res1);
   result := Response;
 finally
  httpClient:= nil;
  httpClient.Free;
  sslIOHandler:= nil;
  sslIOHandler.Free;
  Res1 := nil;
  Res1.Free;
  Params := nil;
  params.Free;
 end;
end;




procedure TfrmMain.Button8Click(Sender: TObject);
var
 data,slResult:Tstringlist;
 temp,venue,marketcount:string;
 i,count:integer;
begin
  leMembefore.Text := inttostr(memused);
   data:=Tstringlist.create;
   slResult:=Tstringlist.create;
   data.Text := listVenues;
   data.Text := stringreplace(data.Text,'},{','},'+#13#10+'{',[rfreplaceall]);

   count := data.count-1; // just show 20 results
   if count > 19 then
    count := 19;

   for i := 0 to count do
     begin
      temp := data.Strings[i];
      venue := copy(temp,pos('"venue":"',temp)+9,length(temp));
      venue := copy(venue,1,pos(',',venue)-1);
      venue := stringreplace(venue,'"','',[rfreplaceall]);
      marketcount :=  copy(temp,pos('"marketCount":',temp)+14,length(temp));
      marketcount := copy(marketcount,1,pos('}',marketcount)-1);
      marketcount := stringreplace(marketcount ,'"','',[rfreplaceall]);
      slResult.Add('Venue :'+venue+' MarketCount :'+marketCount);
     end;

   showmessage(slResult.Text);


 freeandnil(data);
 freeandnil(slResult);
 leMemAfter.Text := inttostr(memused);
end;

Function TfrmMain.GetEvents:string;
 const

 Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/listEvents/';
 UnitedKindom = 'https://api.betfair.com/exchange/betting/rest/v1.0/listEvents/';
var
  httpClient   : TIdHttp ;
  sslIOHandler : TIdSSLIOHandlerSocketOpenSSL ;
  Response     : string ;
  error        : string;
  Filter       : string;
  SourseURL    : string;
  url:string;
  Res,Params :Tstringstream;
  theans,TEMP,EventID,Venue,CountryCode,Timezone,Opendate,MarketCount:string;
  Marketstart,MarketEnd:tdatetime;
  strmarketstart,strmarketend,paramstr:string;
  slResult:tstringlist;
  aLine:string;

begin
   aLine := '';
   slResult:=tstringlist.create;

   error := 'OK';
   Params := TStringStream.Create();
   paramstr:= '{"filter":{"eventTypeIds":["7"]}},"id":"1","jsonrpc":"2.0"}';  // 7 is Horse Racing 1 is soccer see the manual for the different Type Codes
   Params.WriteString(paramstr);
   Res :=  TStringStream.Create();
try
 try
   httpClient := TIdHttp.Create(nil) ;
   httpClient.Request.ContentType := 'application/json;';
   httpClient.Request.Charset := 'utf-8';
   httpClient.Request.ContentLength := sizeof(Params);
   httpClient.Request.Connection    := 'Keep-Alive';
   httpClient.Request.Accept := 'application/json' ;
   httpClient.ReadTimeout    := 60000 ;
   httpClient.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   httpClient.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIOHandler         := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   httpClient.IOHandler := sslIOHandler ;

    case rgServer.ItemIndex of
     0: sourseURL := Australian;
     1: sourseURL := UnitedKindom;
   end;

   httpClient.Post(sourseurl,Params,res);
   except
      on E: Exception do
        begin
         slResult.add(E.Message); //HTTP/1.1 400 Bad Request most likely  means the filter is wrong;
        end;
   end;

   if (res.Size > 0) and (res.Memory <> nil) then
    begin
      theans := StreamToString(res);
      while pos('},{',theans) > 0 do
      begin
        temp := copy(theans,1,pos('},{',theans));
        EventID := copy(temp ,pos('id":"',temp )+5,length(temp ));
        EventID := copy(EventID,1,pos(',',EventID)-1);
        EventID :=  stringreplace(EventID,'"','',[]);
        EventID :=  stringreplace(EventID,'}','',[]);


        Venue := copy(Temp,pos('name":"',Temp)+7,length(Temp));
        Venue := copy(Venue,1,pos(',',Venue)-1);
        Venue :=  stringreplace(Venue,'"','',[]);
        Venue :=  stringreplace(Venue,'}','',[]);

        CountryCode := copy(Temp,pos('countryCode":',Temp)+13,length(Temp));
        CountryCode :=  stringreplace(CountryCode,'"','',[rfReplaceall]);
        CountryCode :=  copy(CountryCode,1,pos(',',CountryCode)-1);
        CountryCode :=  stringreplace(CountryCode,',','',[rfReplaceall]);

        opendate    :=  copy(Temp,pos('openDate":',Temp)+10,length(Temp));
        opendate    :=  copy(opendate,1,pos(',',opendate)-1);
        opendate :=  stringreplace(opendate,'"','',[rfreplaceall]);
        opendate :=  stringreplace(opendate,'}','',[]);

        timezone    :=  copy(Temp,pos('timezone":',Temp)+10,length(Temp));
        timezone    :=  copy(timezone,1,pos(',',timezone)-1);
        timezone    :=  stringreplace(timezone,'"','',[rfreplaceall]);

        aLine := 'EventID='+EventID+',Venue='+ Venue+',CountryCode='+CountryCode+',OpenDate='+opendate+',timezone='+timezone;
        slresult.Add(aLine);
        theans := copy(theans,pos('},{',theans)+3,length(theans));
      end;
      if trim(theAns) <> '' then
        begin
          temp := trim(theans);
          EventID := copy(temp ,pos('id":"',temp )+5,length(temp ));
          EventID := copy(EventID,1,pos(',',EventID)-1);
          EventID :=  stringreplace(EventID,'"','',[]);
          EventID :=  stringreplace(EventID,'}','',[]);

          Venue := copy(Temp,pos('name":"',Temp)+7,length(Temp));
          Venue := copy(Venue,1,pos(',',Venue)-1);
          Venue :=  stringreplace(Venue,'"','',[]);
          Venue :=  stringreplace(Venue,'}','',[]);

          CountryCode := copy(Temp,pos('countryCode":',Temp)+13,length(Temp));
          CountryCode :=  stringreplace(CountryCode,'"','',[rfReplaceall]);
          CountryCode :=  copy(CountryCode,1,pos(',',CountryCode)-1);
          CountryCode :=  stringreplace(CountryCode,',','',[rfReplaceall]);

          opendate    :=  copy(Temp,pos('openDate":',Temp)+10,length(Temp));
          opendate    :=  copy(opendate,1,pos(',',opendate)-1);
          opendate :=  stringreplace(opendate,'"','',[rfreplaceall]);
          opendate :=  stringreplace(opendate,'}','',[]);

          timezone    :=  copy(Temp,pos('timezone":',Temp)+10,length(Temp));
          timezone    :=  copy(timezone,1,pos(',',timezone)-1);
          timezone    :=  stringreplace(timezone,'"','',[rfreplaceall]);
          aLine := 'EventID='+EventID+',Venue='+ Venue+',CountryCode='+CountryCode+',OpenDate='+opendate+',TimeZone='+timezone;
          slresult.Add(aLine);
        end;
    end;
 finally
   result := slResult.text;
   freeandnil(slResult);
   sslIOHandler.Free;
   httpClient.Free;
   Params.Free;
   res.Free;
 end;
end;


function TfrmMain.showUserDetails:string;
const
 Australian = 'https://api-au.betfair.com/exchange/betting/rest/v1.0/listCompetitions/';
 UnitedKindom = 'https://api.betfair.com/exchange/account/rest/v1.0/getAccountDetails/';
var
  httpClient   : TIdHttp ;
  sslIOHandler : TIdSSLIOHandlerSocketOpenSSL ;
  Response     : string ;
  SourseURL    : string;
  paramstr     : string;
  Res1,Params  : Tstringstream;
 begin
 Response := '' ;
 paramstr := '{"filter":{},"id":"1","jsonrpc":"2.0"}';
 Params := TStringStream.Create();
 Params.WriteString(paramstr);
 Res1 :=  TStringStream.Create();

try
  try
   httpClient := TIdHttp.Create(nil) ;
   httpClient.Request.ContentType := 'application/json;';
   httpClient.Request.Charset := 'utf-8';
   httpClient.Request.ContentLength := sizeof(Params);
   httpClient.Request.Connection    := 'Keep-Alive';
   httpClient.Request.Accept := 'application/json' ;
   httpClient.ReadTimeout    := 60000 ;
   httpClient.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;
   httpClient.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIOHandler         := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   httpClient.IOHandler := sslIOHandler ;

   sourseURL := UnitedKindom;

   httpClient.Post(sourseurl,Params,res1);
   except
      on E: Exception do
        begin
         Response := E.Message; //HTTP/1.1 400 Bad Request  //'HTTP/1.1 404 Not Found'
        end;
   end;
   Response := StreamToString(res1);
   result := Response;
 finally
  httpClient:= nil;
  httpClient.Free;
  sslIOHandler:= nil;
  sslIOHandler.Free;
  Res1 := nil;
  Res1.Free;
  Params := nil;
  params.Free;
 end;
end;


procedure TfrmMain.Button9Click(Sender: TObject);
var
 data:tstringlist;
 Answer,currencyCode,firstName,lastName,localeCode,region,timezone,discountRate,pointsBalance:string;

begin
   leMembefore.Text := inttostr(memused);

   data:=tstringlist.Create;
   answer:= showUserDetails;
   currencyCode :=copy(answer,pos('"currencyCode":"',answer)+16,length(answer));
   currencyCode := trim(copy(currencyCode,1,pos(',',currencyCode)-1));
   currencyCode := stringreplace(currencyCode,'"','',[rfreplaceall]);

   firstName := copy(answer,pos('"firstName":"',answer)+13,length(answer));
   firstName := trim(copy(firstName,1,pos(',',firstName)-1));
   firstName := stringreplace(firstName,'"','',[rfreplaceall]);

   lastName := copy(answer,pos('"lastName":"',answer)+12,length(answer));
   lastName := trim(copy(lastName,1,pos(',',lastName)-1));
   lastName := stringreplace(lastName,'"','',[rfreplaceall]);

   localeCode := copy(answer,pos('"localeCode":"',answer)+14,length(answer));
   localeCode := trim(copy(localeCode,1,pos(',',localeCode)-1));
   localeCode := stringreplace(localeCode,'"','',[rfreplaceall]);

   region     := copy(answer,pos('"region":"',answer)+10,length(answer));
   region     := trim(copy(region,1,pos(',',region)-1));
   region     := stringreplace(region,'"','',[rfreplaceall]);

   timezone     := copy(answer,pos('"timezone":"',answer)+12,length(answer));
   timezone     := trim(copy(timezone,1,pos(',',timezone)-1));
   timezone     := stringreplace(timezone,'"','',[rfreplaceall]);


   discountRate     := copy(answer,pos('"discountRate":',answer)+15,length(answer));
   discountRate     := trim(copy(discountRate,1,pos(',',discountRate)-1));
   discountRate     := stringreplace(discountRate,'"','',[rfreplaceall]);

   pointsBalance     := copy(answer,pos('"pointsBalance":',answer)+16,length(answer));
   pointsBalance     := trim(copy(pointsBalance,1,pos('}',pointsBalance)-1));
   pointsBalance     := stringreplace(pointsBalance,'"','',[rfreplaceall]);

 //'{"currencyCode":"AUD","firstName":"Carl","lastName":"Nielsen","localeCode":"en","region":"AUS_NZL","timezone":"Australia/Queensland","discountRate":0.0,"pointsBalance":719}'

   data.Add('CurrencyCode : '+currencyCode);
   data.Add('FirstName    : '+firstname);
   data.Add('SurName      : '+lastname);
   data.Add('LocaleCode   : '+LocaleCode);
   data.Add('Region       : '+ region);
   data.Add('Timezone     : '+timezone);
   data.Add('DiscountRate : '+ discountRate);
   data.Add('PointsBalance: '+pointsBalance);


   showmessage(data.Text);
   freeandnil(data);
   leMemAfter.Text := inttostr(memused);
end;

function LogOutBetFair:boolean;
// this code is adapted from the example code on the website
var
  http      : TIdHttp ;
  sslIO     : TIdSSLIOHandlerSocketOpenSSL ;
  Answer    : string ;
begin
 result := false ;
  try
   Answer  := '' ;
   http      := TIdHttp.Create(nil) ;

   http.Request.Accept := 'application/json' ;
   http.ReadTimeout    := 60000 ;
   http.Request.CustomHeaders.Add('X-Authentication:' + SessionToken ) ;

   if App_Key <> '' then http.Request.CustomHeaders.Add('X-Application:' + App_Key ) ;
   sslIO         := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
   http.IOHandler := sslIO;
   Answer := http.Get('https://identitysso.betfair.com/api/logout') ;
   if pos('SUCCESS',Answer) > 0 then
    result := true;
  finally
    http := nil;
    http.Free ;
    sslIO:= nil;
    sslIO.free ;
  end ;
end ;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if LoggedIn then
  begin
    if LogOutBetFair then
     showmessage('You are Logged out from BetFair');
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  slAppKeys:Tstringlist;
  Path,Temp:string;
  filename:string;
  i:Integer;
begin
  LoggedIn := false;

  Path := extractfilepath(application.ExeName);
  filename := 'AppKeys.txt';
  slAppKeys:=Tstringlist.create;
  leAppKey.Text :='';
  leDelayedAppKey.Text := '';
  if fileexists(path+filename) then    // if the app keys have bben saved recover them and fill the app key form and variables
   begin
     slAppkeys.LoadFromFile(path+filename);
     if slAppkeys.Count > 0 then // if the file is blank then don't populate the form
     begin
     for I := 0 to slAppkeys.Count -1 do
      begin
        if pos('<APP_KEY>',slappkeys.Strings[i]) > 0 then
         begin
          Temp := slappkeys.Strings[i];
          leAppKey.Text := trim(copy(Temp,pos('>',Temp)+1,length(temp)));
          App_Key := trim(leAppKey.Text);  //<- these are golbal variables  for use in all requests
         end;
        if pos('<DELAYED_APP_KEY>',slappkeys.Strings[i]) > 0 then
         begin
          Temp := slappkeys.Strings[i];
          leDelayedAppKey.Text := trim(copy(Temp,pos('>',Temp)+1,length(temp)));
          DelayedAppKey := trim(leDelayedAppKey.Text);   //<- these are golbal variables  for use in some requests
         end;
      end;
     end;
   end;
  freeandnil(slappkeys);

  if not loggedin then
    btnLoginClick(nil);

end;

end.
