object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 475
  ClientWidth = 704
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
    Top = 434
    Width = 704
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 404
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
  end
  object WebBrowser1: TWebBrowser
    Left = 0
    Top = 77
    Width = 704
    Height = 357
    Align = alClient
    TabOrder = 1
    OnBeforeNavigate2 = WebBrowser1BeforeNavigate2
    ExplicitTop = 72
    ExplicitHeight = 332
    ControlData = {
      4C000000C3480000E62400000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 704
    Height = 77
    Align = alTop
    Color = clInfoBk
    Lines.Strings = (
      
        'Note:  The web browser may be slow in retrieving the security UR' +
        'L, so there may be a pause depending on the speed of the interne' +
        't. '
      ''
      
        ' Also: If after login, if the form doesn'#8217't close and return to t' +
        'he main form the problem may be to do with the WebBrowser1Before' +
        'Navigate2 '
      
        'event not firing...  Try replacing the TWebbrowser component wit' +
        'h your version.')
    TabOrder = 2
  end
end
