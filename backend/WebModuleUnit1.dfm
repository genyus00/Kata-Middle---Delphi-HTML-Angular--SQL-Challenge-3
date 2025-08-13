object WebModule1: TWebModule1
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModuleDefaultHandlerAction
    end
    item
      Name = 'Ahello'
      PathInfo = '/hello'
      OnAction = WebModuleWebActionItem1Action
    end
    item
      Name = 'AGetLog'
      PathInfo = '/GetLog'
      OnAction = WebModule1AGetLogAction
    end>
  BeforeDispatch = WebModuleBeforeDispatch
  Height = 326
  Width = 415
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
    Filters = <>
    OnFormatResult = DSHTTPWebDispatcher1FormatResult
    WebDispatch.PathInfo = 'datasnap*'
    Left = 96
    Top = 75
  end
  object WebFileDispatcher1: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end
      item
        MimeType = 'text/html'
        Extensions = 'htm;html'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpg;jpeg;jpe'
      end
      item
        MimeType = 'image/gif'
        Extensions = 'gif'
      end
      item
        MimeType = 'text/plain'
        Extensions = 'txt'
      end
      item
        MimeType = 'text/plain'
        Extensions = 'log'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirInclude
        DirectoryMask = '/Bicacoras'
      end>
    RootDirectory = '.'
    VirtualPath = '/files'
    Left = 96
    Top = 136
  end
end
