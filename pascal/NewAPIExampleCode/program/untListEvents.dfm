object frmListEvents: TfrmListEvents
  Left = 0
  Top = 0
  Caption = 'frmListEvents'
  ClientHeight = 518
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 460
    Height = 477
    Align = alClient
    Columns = <
      item
        Caption = 'Event ID'
        Width = 60
      end
      item
        Caption = 'Venue'
        Width = 80
      end
      item
        Caption = 'CTRY'
        Width = 60
      end
      item
        Caption = 'OpenDate'
        Width = 120
      end
      item
        Caption = 'TimeZone'
        Width = 100
      end>
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitTop = 2
    ExplicitWidth = 562
    ExplicitHeight = 253
  end
  object Panel1: TPanel
    Left = 0
    Top = 477
    Width = 460
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 259
    ExplicitWidth = 562
    object Button1: TButton
      Left = 24
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
    end
  end
end
