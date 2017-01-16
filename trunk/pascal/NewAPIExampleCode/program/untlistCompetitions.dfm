object frmListCompetitions: TfrmListCompetitions
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'ListCompetitions'
  ClientHeight = 476
  ClientWidth = 496
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
    Top = 435
    Width = 496
    Height = 41
    Align = alBottom
    TabOrder = 0
    object Button1: TButton
      Left = 16
      Top = 6
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
    Width = 496
    Height = 435
    Align = alClient
    Columns = <
      item
        Caption = 'ID'
        Width = 70
      end
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Caption = 'Count'
        Width = 60
      end
      item
        Caption = 'Region'
        Width = 80
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
end
