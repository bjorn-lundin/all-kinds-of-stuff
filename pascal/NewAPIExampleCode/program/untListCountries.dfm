object frmlistCountries: TfrmlistCountries
  Left = 0
  Top = 0
  Caption = 'List Countries'
  ClientHeight = 294
  ClientWidth = 562
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
  object Panel1: TPanel
    Left = 0
    Top = 253
    Width = 562
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitLeft = 144
    ExplicitTop = 248
    ExplicitWidth = 185
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
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 562
    Height = 253
    Align = alClient
    Columns = <
      item
      end
      item
      end
      item
      end>
    TabOrder = 1
    ViewStyle = vsReport
    ExplicitTop = -6
  end
end
