unit frmfirst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  pqconnection, sqldb, db, BufDataset,  FileUtil,
  Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, DbCtrls, Menus, ComCtrls, TAGraph, TASeries,
  TAChartUtils, sql, LCLType;

type

  { TfrmDataview }

  TfrmDataview = class(TForm)
    btnDataset: TButton;
    Chart: TChart;
    Label1: TLabel;
    lblNumRecords: TLabel;
    Statusbar: TStatusBar;
    Tree: TTreeView;
    procedure btnDatasetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDataview: TfrmDataview;
  PGConn  : TPQConnection;

implementation

{$R *.lfm}

{ TfrmDataview }




procedure TfrmDataview.btnDatasetClick(Sender: TObject);
var
  T       : TSQLTransaction;
  Query   : TSQLQuery;
  Marketid : String;
  TS,Old_Ts : TDatetime;
  S : AnsiString;
  Current_Node : TTreeNode;
  sSql : string;
begin
  T := sql.CreateTransaction(PGConn);
  Query := sql.CreateQuery(T);
  sSql := 'select STARTTS::date as STARTTS, MARKETID ';
  sSql += 'from AMARKETS ';
  sSql += 'where MARKETTYPE = ''WIN'' ';
  sSql += 'order by STARTTS';
  Query.SQL.Text := sSql;
  T.StartTransaction;
  Statusbar.Panels[0].Text:= 'Adding markets';
  Statusbar.Update;
  Query.Open;
  Old_Ts := Now+100;

  while not Query.Eof do begin
    Marketid := Query.FieldByName('MARKETID').AsString;
    Ts := Query.FieldByName('STARTTS').AsDateTime;
    if Ts <> Old_Ts then begin
      DateTimeToString (S,'Mmm-dd',TS);
      Current_Node := Tree.Items.AddChild(Tree.TopItem, S);
      Old_Ts := Ts;
    end;
    Tree.Items.AddChild(Current_Node, Marketid);
    Query.Next;
  end;
  Statusbar.Panels[0].Text:= '';
  Statusbar.Update;
  Query.Close;
  Query.Free;
  T.Commit;
  T.Free;
end;



procedure TfrmDataview.FormCreate(Sender: TObject);

begin
  PGConn := Sql.CreateConnection;
  PGConn.Open;
  Tree.Items.Add (nil,'Marketids');
  btnDatasetClick(Sender); // fill the tree
  KeyPreview := True; // so we can listen to keystokes in the form
end;

procedure TfrmDataview.FormDestroy(Sender: TObject);
begin
   PGConn.Close;
end;

procedure TfrmDataview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Self.Close;
end;


function Is_Winner (var T           : TSQLTransaction;
                        Marketid    : String;
                        Selectionid : Longint) : Boolean;
var
  Get_Is_Winner   : TSQLQuery;
  sSql : string;
begin

  Get_Is_Winner := sql.CreateQuery(T);
  sSql := 'select * from ARUNNERS ';
  sSql += 'where MARKETID = :MARKETID ';
  sSql += 'and SELECTIONID = :SELECTIONID';
  Get_Is_Winner.SQL.Text := sSql;
  Get_Is_Winner.Prepare;

  Get_Is_Winner.Params.ParamByName('MARKETID').AsString:=Marketid;
  Get_Is_Winner.Params.ParamByName('SELECTIONID').AsLargeInt := Selectionid;
  Get_Is_Winner.Open;

  if not Get_Is_Winner.EOF then
    Result := Get_Is_Winner.FieldByName('STATUS').AsString = 'WINNER'
  else
    Result := False ;

  Get_Is_Winner.Close;
  Get_Is_Winner.Free;
end;




procedure TfrmDataview.TreeClick(Sender: TObject);
var
  T       : TSQLTransaction;
  Get_Selectionid   : TSQLQuery;
  Get_Prices   : TSQLQuery;

  Marketid : String;
  Selectionid : Longint;
  sSql : string;
  Layprice,Backprice : Double;
  TS : TDatetime;
  //Series : TLineSeries;
  S : AnsiString;
  i : longint;
  Colors: array[1..10] of TColor = (clRed, clYellow, clFuchsia, clGreen, clBlue,
                                       clMaroon, clOlive, clBlack, clGray, clAqua);
  Color_Index :Integer;
  Series_Array_Back : array[1..36] of TLineSeries;
  Series_Array_Lay : array[1..36] of TLineSeries;


begin

  if Tree.Selected <> nil then
    begin
      Marketid := Tree.Selected.Text ;
      i := Pos('1.', Marketid);
      if i = 0 then exit;
    end
  else
    exit;

  Chart.ClearSeries;
  chart.LeftAxis.Marks.LabelFont.Name := 'Arial';
  chart.LeftAxis.Marks.LabelFont.Size := 10;
  chart.LeftAxis.Marks.LabelFont.Orientation := 450;
  chart.LeftAxis.Marks.Frame.Visible := false;
  chart.LeftAxis.Marks.Frame.Style := psSolid;
  //chart.LeftAxis.Marks.Frame.FPColor := colBlack;
  //chart.LeftAxis.Grid.FPColor := colDkGray;
  chart.BottomAxis.Marks.Visible := false;
  //chart.BottomAxis.Grid.FPColor := colDkGray;
  chart.Color := $FFA0A0;
  chart.BackColor := $FFFFFF;
  //chart.LeftAxis.Marks.Range.Max:=5;
  //chart.LeftAxis.Marks.Range.UseMax:=True;
  T := sql.CreateTransaction(PGConn);
  Get_Selectionid := sql.CreateQuery(T);
  Get_Selectionid.SQL.Text := 'select SELECTIONID from ARUNNERS where MARKETID=:MARKETID';
  Get_Prices := sql.CreateQuery(T);
  sSql := 'select * from APRICESHISTORY ';
  sSql += 'where MARKETID=:MARKETID ' ;
  sSql += 'and SELECTIONID = :SELECTIONID ';
  sSql += 'order by PRICETS' ;
  Get_Prices.SQL.Text := sSql;
  T.StartTransaction;
  Statusbar.Panels[0].Text:= 'Filling Chart';
  Statusbar.Update;
  Get_Selectionid.Prepare;
  Get_Selectionid.Params.ParamByName('MARKETID').AsString:=Marketid;
  Get_Selectionid.Open;
  Get_Prices.Prepare;
  Color_Index := 0;

  Get_Selectionid.Last;
  lblNumRecords.Caption:= IntToStr(Get_Selectionid.RecordCount);
  Get_Selectionid.First;


  while not Get_Selectionid.Eof do begin
    Color_Index := Color_Index +1;
    Series_Array_Back[Color_Index] := TLineSeries.Create(Chart);
    Series_Array_Back[Color_Index].Marks.Style := TSeriesMarksStyle(smsValue); //or smsLabelValue
    Series_Array_Back[Color_Index].Marks.Distance:= 5;
    Series_Array_Back[Color_Index].ShowPoints:= False;
    Series_Array_Back[Color_Index].Marks.Visible:=False;
    Series_Array_Back[Color_Index].LinePen.Style := psSolid;

    Series_Array_Lay[Color_Index] := TLineSeries.Create(Chart);
    Series_Array_Lay[Color_Index].Marks.Style := TSeriesMarksStyle(smsValue); //or smsLabelValue
    Series_Array_Lay[Color_Index].Marks.Distance:= 5;
    Series_Array_Lay[Color_Index].ShowPoints:= False;
    Series_Array_Lay[Color_Index].Marks.Visible:=False;
    Series_Array_Lay[Color_Index].LinePen.Style := psDot;
    i := 0;
    Selectionid := Get_Selectionid.FieldByName('SELECTIONID').AsLargeInt;
    Get_Prices.Params.ParamByName('MARKETID').AsString := Marketid;
    Get_Prices.Params.ParamByName('SELECTIONID').AsLargeInt := Selectionid;
    Get_Prices.Open;

    if Is_Winner(T, Marketid, Selectionid) then begin
      Series_Array_Back[Color_Index].SeriesColor:= clGreen;
      Series_Array_Lay[Color_Index].SeriesColor:= clGreen;
    end
    else begin
      Series_Array_Back[Color_Index].SeriesColor:= clRed;
      Series_Array_Lay[Color_Index].SeriesColor:= clRed;
    end;

    while not Get_Prices.Eof do begin
      i := i+1;
      Layprice := Get_Prices.FieldByName('LAYPRICE').AsFloat;
      Backprice := Get_Prices.FieldByName('BACKPRICE').AsFloat;
      TS := Get_Prices.FieldByName('PRICETS').AsDateTime;
      // insert to graph
      DateTimeToString (S,'hh:nn',TS);

      Series_Array_Back[Color_Index].AddXY(i, Backprice, S, Colors[Color_Index]);
      Series_Array_Lay[Color_Index].AddXY(i, Layprice, S, Colors[Color_Index]);
      Get_Prices.Next;
    end;
    Get_Selectionid.Next;
    Get_Prices.Close;
    Chart.AddSeries(Series_Array_Back[Color_Index]);
    Chart.AddSeries(Series_Array_Lay[Color_Index]);
  //  Chart.AxisList.BottomAxis.Marks.Source := Series_Array[Color_Index].Source;
    Chart.AxisList.BottomAxis.Marks.Style := smsLabel;
    //Series.Free;
  end;
  Chart.Update;
  //Series.Free;
//  for i := i to length(Series_Array) do Series_Array[i].Free;

  Statusbar.Panels[0].Text:= '';
  Statusbar.Update;
  Get_Selectionid.Close;
  Get_Selectionid.Free;
  Get_Prices.Free;
  T.Commit;
  T.Free;
end;




end.

