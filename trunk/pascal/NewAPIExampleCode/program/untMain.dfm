object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Example BetFair Delphi Project'
  ClientHeight = 437
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 411
    Height = 65
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 914
    object GroupBox4: TGroupBox
      Left = 1
      Top = 1
      Width = 304
      Height = 63
      Align = alLeft
      Caption = 'Memory '
      TabOrder = 0
      object leMemBefore: TLabeledEdit
        Left = 3
        Top = 37
        Width = 86
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'Mem Before'
        TabOrder = 0
      end
      object leMemAfter: TLabeledEdit
        Left = 95
        Top = 37
        Width = 75
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Mem After'
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 65
    Width = 185
    Height = 372
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 463
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 183
      Height = 168
      Align = alTop
      TabOrder = 0
      object GroupBox1: TGroupBox
        Left = 1
        Top = 1
        Width = 181
        Height = 166
        Align = alClient
        Caption = 'Add your White Listed App Keys'
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = -1
        object leAppKey: TLabeledEdit
          Left = 3
          Top = 40
          Width = 166
          Height = 21
          EditLabel.Width = 40
          EditLabel.Height = 13
          EditLabel.Caption = 'App Key'
          TabOrder = 0
        end
        object LabeledEdit1: TLabeledEdit
          Left = -157
          Top = 424
          Width = 166
          Height = 21
          EditLabel.Width = 82
          EditLabel.Height = 13
          EditLabel.Caption = 'Delayed App Key'
          TabOrder = 1
        end
        object leDelayedAppKey: TLabeledEdit
          Left = 3
          Top = 88
          Width = 166
          Height = 21
          EditLabel.Width = 82
          EditLabel.Height = 13
          EditLabel.Caption = 'Delayed App Key'
          TabOrder = 2
        end
        object Button1: TButton
          Left = 3
          Top = 128
          Width = 75
          Height = 25
          Caption = 'Save'
          TabOrder = 3
          OnClick = Button1Click
        end
      end
    end
    object GroupBox3: TGroupBox
      Left = 1
      Top = 169
      Width = 183
      Height = 200
      Align = alTop
      Caption = 'Login'
      TabOrder = 1
      object btnLogin: TButton
        Left = 3
        Top = 160
        Width = 175
        Height = 25
        Caption = 'Log In to BetFair '
        TabOrder = 0
        OnClick = btnLoginClick
      end
      object Memo1: TMemo
        Left = 2
        Top = 15
        Width = 179
        Height = 139
        Align = alTop
        Color = clInfoBk
        Lines.Strings = (
          'This is the standard interactive '
          'BetFair Login. It is a BetFair '
          'requirement that this website be '
          'used for the Login.'
          ''
          'You can Login without App Keys '
          'but no information can be '
          'retrieved without the White Listed '
          'App Keys')
        TabOrder = 1
      end
    end
  end
  object Panel4: TPanel
    Left = 185
    Top = 65
    Width = 224
    Height = 372
    Align = alLeft
    TabOrder = 2
    ExplicitHeight = 463
    object GroupBox2: TGroupBox
      Left = 1
      Top = 1
      Width = 222
      Height = 370
      Align = alClient
      Caption = 'Basic Routines'
      Enabled = False
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitTop = 5
      object Button3: TButton
        Left = 13
        Top = 89
        Width = 180
        Height = 25
        Caption = 'listCompetitions'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 13
        Top = 120
        Width = 180
        Height = 25
        Caption = 'listCountries'
        TabOrder = 1
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 13
        Top = 151
        Width = 180
        Height = 25
        Caption = 'listEvents'
        TabOrder = 2
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 13
        Top = 183
        Width = 177
        Height = 25
        Caption = 'listMarketTypes'
        TabOrder = 3
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 13
        Top = 214
        Width = 177
        Height = 25
        Caption = 'listTimeRanges'
        TabOrder = 4
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 13
        Top = 246
        Width = 177
        Height = 25
        Caption = 'listVenues'
        TabOrder = 5
        OnClick = Button8Click
      end
      object rgServer: TRadioGroup
        Left = 2
        Top = 15
        Width = 218
        Height = 68
        Align = alTop
        Caption = 'Server'
        ItemIndex = 0
        Items.Strings = (
          'Australian Server'
          'UK Server')
        TabOrder = 6
      end
      object Button9: TButton
        Left = 13
        Top = 277
        Width = 177
        Height = 25
        Caption = 'getAccountDetails'
        TabOrder = 7
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 13
        Top = 309
        Width = 177
        Height = 25
        Caption = 'getAccountFunds'
        TabOrder = 8
        OnClick = Button10Click
      end
    end
  end
end
