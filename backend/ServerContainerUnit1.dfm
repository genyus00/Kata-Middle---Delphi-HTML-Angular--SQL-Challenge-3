object ServerContainer1: TServerContainer1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSAuthenticationManager1: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManager1UserAuthenticate
    OnUserAuthorize = DSAuthenticationManager1UserAuthorize
    Roles = <>
    Left = 96
    Top = 157
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 96
    Top = 91
  end
end
