object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 569
  ClientWidth = 1062
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1062
    Height = 52
    Align = alTop
    TabOrder = 0
    object BtnListEmployees: TButton
      Left = 11
      Top = 6
      Width = 103
      Height = 35
      Caption = 'List Employees'
      TabOrder = 0
      OnClick = BtnListEmployeesClick
    end
    object BtnListCustomers: TButton
      Left = 147
      Top = 6
      Width = 103
      Height = 35
      Caption = 'List Customers'
      TabOrder = 1
      OnClick = BtnListCustomersClick
    end
  end
  object PnlMain: TPanel
    Left = 0
    Top = 52
    Width = 1062
    Height = 517
    Align = alClient
    Caption = 'PnlMain'
    TabOrder = 1
    object MemOutput: TMemo
      Left = 1
      Top = 1
      Width = 274
      Height = 307
      Align = alLeft
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
    object GrdEmployee: TDBGrid
      Left = 275
      Top = 1
      Width = 786
      Height = 307
      Align = alClient
      DataSource = DsEmployee
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      OnDblClick = GrdEmployeeDblClick
    end
    object GrdCustomer: TDBGrid
      Left = 1
      Top = 308
      Width = 1060
      Height = 208
      Align = alBottom
      DataSource = DsCustomer
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      OnDblClick = GrdCustomerDblClick
      Columns = <
        item
          Expanded = False
          FieldName = 'CustNo'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Company'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Phone'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'FAX'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'TaxRate'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Contact'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'LastInvoiceDate'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'FullAddress'
          Visible = True
        end>
    end
  end
  object DsEmployee: TDataSource
    DataSet = QuEmployee
    Left = 70
    Top = 67
  end
  object DsCustomer: TDataSource
    DataSet = QuCustomer
    Left = 166
    Top = 66
  end
  object QuEmployee: TClientDataSet
    PersistDataPacket.Data = {
      890700009619E0BD010000001800000006002A00000003000000DB0005456D70
      4E6F0400010000000000084C6173744E616D6501004900000001000557494454
      480200020014000946697273744E616D65010049000000010005574944544802
      0002000F000850686F6E65457874010049000000010005574944544802000200
      040008486972654461746508000800000000000653616C617279080004000000
      000003000D44454641554C545F4F52444552020082000100000001000B505249
      4D4152595F4B455902008200010000000100044C434944040001000904000000
      000002000000064E656C736F6E07526F626572746F033235300000EA4F4F87CC
      42000000000088E3400000000400000005596F756E6705427275636503323333
      0000EA4F4F87CC42000000008019EB4000000005000000074C616D6265727403
      4B696D02323200001A4FB687CC4200000000006AD84000000008000000074A6F
      686E736F6E064C65736C696503343130000086A74B88CC42000000008076D840
      0000000900000006466F72657374045068696C033232390000AE8D6A88CC4200
      0000008076D8400000000B00000006576573746F6E054B2E204A2E0233340000
      18A82E8BCC42000000009E41E0400000000C000000034C656505546572726903
      3235360000C8723A8CCC42000000008022E6400000000E0000000448616C6C07
      53746577617274033232370000E4FE918CCC420000000054D6E0400000000F00
      000005596F756E67094B6174686572696E65033233310000B0BEAB8CCC420000
      000000D4D740000000140000000C50617061646F706F756C6F73054368726973
      0338383700003875058BCC42000000008076D840000000180000000646697368
      65720450657465033838380000DC7C938DCC42000000000080D6400000001C00
      00000642656E6E657403416E6E013500006020018FCC429A99999959D6E04000
      00001D00000008446520536F757A6105526F6765720332383800006EE62C8FCC
      420000000000E7D840000000220000000742616C6477696E054A616E65740132
      000000B97C8FCC420000000000C1D64000000024000000065265657665730552
      6F676572013600004AD8D68FCC4200000000806AE04000000025000000095374
      616E73627572790657696C6C6965013700004AD8D68FCC42000000000027E340
      0000002C0000000550686F6E67064C65736C69650332313600004C443B90CC42
      00000000C0B3E3400000002D0000000A52616D616E617468616E054173686F6B
      033230390000E62FD390CC4248E17A149E41E0400000002E0000000853746561
      646D616E0657616C74657203323130000056C9E790CC4200000000C023D34000
      000034000000094E6F72647374726F6D054361726F6C0334323000000AD57291
      CC42000000000094B1400000003D000000054C65756E67044C756B6501330000
      04BFD892CC420000000080D8E04000000041000000074F27427269656E085375
      6520416E6E65033837370000204B3093CC4200000000C08ADE40000000470000
      000742757262616E6B0B4A656E6E69666572204D2E03323839000042846B93CC
      42000000008022E640004000480000000A5375746865726C616E6407436C6175
      646961000028647893CC4200000000606EE1400000005300000006426973686F
      700444616E61033239300000B489E493CC420000000000F9E540000000550000
      00094D6163446F6E616C64074D61727920532E033437370000B489E493CC4200
      000000606EE1400000005E0000000857696C6C69616D730552616E6479033839
      320000ECA19394CC42000000000039DC40000000690000000642656E64657209
      4F6C6976657220482E033235350000E2B33095CC4200000000E0F7E140000000
      6B00000004436F6F6B054B6576696E033839340000BA645B96CC420000000080
      55E1400000006D0000000542726F776E054B656C6C79033230320000441E6396
      CC4200000000005EDA400000006E000000064963686964610459756B69023232
      0000441E6396CC42000000004016D940000000710000000450616765044D6172
      790338343500004EA30F97CC42000000000070E7400000007200000006506172
      6B65720442696C6C0332343700004A629097CC42000000000017E14000000076
      0000000859616D616D6F746F0754616B617368690232330000AEA1DD97CC4200
      00000000BDDF4000000079000000074665727261726907526F626572746F0131
      0000A8F4F997CC420000000080C6E3400000007F0000000859616E6F77736B69
      074D69636861656C033439320000B00D4298CC4200000000007CE54000400086
      00000004476C6F6E074A6163717565730000341A6698CC4200000000C045D840
      00000088000000074A6F686E736F6E0553636F7474033236350000FA2C9C98CC
      42C3F5285C3FDFDD400000008A00000005477265656E04542E4A2E0332313800
      00C8581A99CC42000000000094E1400040008D000000074F73626F726E650650
      696572726500001A91BC99CC42000000000062E140000000900000000A4D6F6E
      74676F6D657279044A6F686E0338323000008E029A9ACC4200000000606EE140
      000000910000000C4775636B656E6865696D6572044D61726B0332323100007C
      FBEE9ACC42000000000040DF40}
    Active = True
    Aggregates = <>
    FieldOptions.AutoCreateMode = acCombineAlways
    FieldOptions.PositionMode = poFirst
    FieldOptions.UpdatePersistent = True
    Params = <>
    Left = 64
    Top = 152
    object QuEmployeeFullName: TStringField
      FieldKind = fkCalculated
      FieldName = 'FullName'
      Size = 80
      Calculated = True
    end
  end
  object QuCustomer: TClientDataSet
    PersistDataPacket.Data = {
      451D00009619E0BD01000000180000000D003700000003000000910106437573
      744E6F080004000000000007436F6D70616E7901004900000001000557494454
      48020002001E000541646472310100490000000100055749445448020002001E
      000541646472320100490000000100055749445448020002001E000443697479
      0100490000000100055749445448020002000F00055374617465010049000000
      0100055749445448020002001400035A69700100490000000100055749445448
      020002000A0007436F756E747279010049000000010005574944544802000200
      14000550686F6E650100490000000100055749445448020002000F0003464158
      0100490000000100055749445448020002000F00075461785261746508000400
      0000000007436F6E746163740100490000000100055749445448020002001400
      0F4C617374496E766F69636544617465080008000000000003000D4445464155
      4C545F4F52444552020082000100000001000B5052494D4152595F4B45590200
      8200010000000100044C43494404000100090400000000000000000000000014
      9340114B6175616920446976652053686F70706513342D393736205375676172
      6C6F616620487779095375697465203130330B4B61706161204B617561690248
      490A39343736362D313233340255530C3830382D3535352D303236390C383038
      2D3535352D3032373800000000000021400C4572696361204E6F726D616E000C
      DBC6B59DCC42004014000000000000003C934006556E6973636F0C504F20426F
      78205A2D3534370846726565706F727407426168616D61730C3830392D353535
      2D333931350C3830392D3535352D3439353800000000000000000F47656F7267
      6520576561746865727300D497E9F09CCC42004014000000000000001C95400B
      53696768742044697665720E31204E657074756E65204C616E650B4B61746F20
      506170686F73064379707275730C3335372D362D3837363730380C3335372D36
      2D38373039343300000000000000000F5068796C6C69732053706F6F6E657200
      181638A49CCC42004014000000000000002895401D4361796D616E2044697665
      727320576F726C6420556E6C696D697465640A504F20426F78203534310C4772
      616E64204361796D616E1342726974697368205765737420496E646965730C30
      31312D352D3639373034340C3031312D352D3639373036340000000000000000
      0A4A6F65204261696C657900E0F509A892CC4200400000000000000000309540
      18546F6D2053617779657220446976696E672043656E747265153633322D3120
      54686972642046727964656E686F6A0D43687269737469616E73746564095374
      2E2043726F69780530303832301155532056697267696E2049736C616E64730C
      3530342D3739382D333032320C3530342D3739382D3737373200000000000000
      000C43687269732054686F6D617300F01A992993CC4200000000000000000000
      90954015426C7565204A61636B20417175612043656E7465721632332D373338
      2050616464696E67746F6E204C616E6509537569746520333130075761697061
      68750248490539393737360255530C3430312D3630392D373632330C3430312D
      3630392D3934303300000000000000000E45726E657374204261727261747400
      8078B9DA9CCC4200400000000000000000A095400F5649502044697665727320
      436C75620B3332204D61696E2053742E0D43687269737469616E737465640953
      742E2043726F69780530323830301155532056697267696E2049736C616E6473
      0C3830392D3435332D353937360C3830392D3435332D35393332000000000000
      00001352757373656C6C204368726973746F7068657200DC0E19B59DCC420040
      00000000000000009897400E4F6365616E2050617261646973650B504F20426F
      7820383734350B4B61696C75612D4B6F6E610248490539343735360255530C38
      30382D3535352D383233310C3830382D3535352D383435300000000000000000
      0C5061756C20476172646E6572005882F0DA9CCC4200401400000000000000A4
      97401446616E7461737469717565204171756174696361145A33322039393920
      233132412D373720412E412E06426F676F746108436F6C756D6269610C303537
      2D312D3737333433340C3035372D312D37373334323100000000000000000A53
      7573616E20576F6E670064071BB79BCC42004000000000000000003C9840124D
      61726D6F742044697665727320436C75620D38373220517565656E2053742E09
      4B69746368656E6572074F6E746172696F0747334E203245310643616E616461
      0C3431362D3639382D303339390C3432362D3639382D30333939000000000000
      00000B4A6F796365204D617273680008486FE78CCC4200400000000000000000
      609840105468652044657074682043686172676515313532343320556E646572
      7761746572204677792E084D61726174686F6E02464C0533353030330255530C
      3830302D3535352D333739380C3830302D3535352D3033353300000000000000
      000F53616D2057697468657273706F6F6E0030E2A4B49ACC4200400000000000
      0000006C98400B426C75652053706F727473153230332031327468204176652E
      20426F7820373436094769726962616C6469024F520539313138370255530C36
      31302D3737322D363730340C3631302D3737322D363839380000000000000000
      0D54686572657361204B756E656300F80031FC8FCC4200400000000000000000
      609940104D616B616920534355424120436C75620B504F20426F782038353334
      0B4B61696C75612D4B6F6E610248490539343735360255530C3331372D363439
      2D393039380C3331372D3634392D3637383700000000000000000B446F6E6E61
      2053696175730028DE34749DCC4200400000000000000000B499400B41637469
      6F6E20436C75620D504F20426F7820353435312D460853617261736F74610246
      4C0533323237340255530C3831332D3837302D303233390C3831332D3837302D
      303238320000000000000000104D69636861656C20537075726C696E67009812
      D5F798CC4200401000000000000000CC9940144A616D61696361205343554241
      2043656E74726509504F20426F78203638064E656772696C074A616D61696361
      0B5765737420496E646965730C3031312D332D3639373034330C3031312D332D
      36393730343300000000000000000E426172626172612048617276657900380E
      71BF94CC4200400000000000000000409A400E49736C616E642046696E646572
      73153631333320312F332053746F6E65204176656E75650E53742053696D6F6E
      732049736C650247410533323532310255530C3731332D3432332D353637350C
      3731332D3432332D3536373600000000000000000E4465736D6F6E64204F7274
      65676100901CDD669DCC4200401400000000000000009F4012416476656E7475
      726520556E6465727365610A504F20426F78203734340B42656C697A65204369
      74790642656C697A650C3031312D33342D30393035340C3031312D33342D3039
      30363400000000000000000F476C6F72696120476F6E7A616C657300980FD9D4
      9CCC42004000000000000000008CA04010426C75652053706F72747320436C75
      62163633333635204E657A20506572636520537472656574054C6172676F0246
      4C0533343638340255530C3631322D3839372D303334320C3631322D3839372D
      3033343800000000000000000E48617272792042617468626F6E65006864839D
      8CCC4200400000000000000000AEA040154672616E6B27732044697665727320
      537570706C791331343535204E6F72746820343474682053742E06457567656E
      65024F520539303432370255530C3530332D3535352D323737380C3530332D35
      35352D3237363900000000000000000D4C6C6F79642046656C6C6F777300C82E
      C61A89CC4200400000000000000000D8A0401244617679204A6F6E657327204C
      6F636B65721432343620536F757468203136746820506C6163650956616E636F
      75766572024243074B3856203950310643616E6164610C3830332D3530392D30
      3131320C3830332D3530392D3035353300000000000000000C54616E79612057
      61676E65720058886ACA91CC4200401400000000000000E6A0400C5343554241
      2048656176656E0D504F20426F7820512D38383734064E617373617507426168
      616D61730C3031312D33322D30393438350C3031312D33322D30393438350000
      00000000000010526F62657274204D696368656C696E6400B0DAC69E8ECC4200
      401400000000000000EAA040185368616E6772692D4C612053706F7274732043
      656E7465720D504F20426F7820442D353439350846726565706F727407426168
      616D61730C3031312D33322D30383537340C3031312D33322D34343933380000
      0000000000000E4672616E6B2050616E69616775610074867D3289CC42004010
      0000000000000016A24015446976657273206F6620436F7266752C20496E632E
      114D61726D6F73657420506C6163652035340F4179696F73204D617474686169
      6F7305436F726675064772656563650C33302D3636312D38383336340C33302D
      3636312D303539343300000000000000000D436861726C6573204C6F70657A00
      4028065C9BCC420040000000000000000064A240104B69726B20456E74657270
      72697365730C34322041717561204C616E6507486F7573746F6E025458053737
      3037390255530C3731332D3535362D363433370C3731332D3535362D31303733
      00000000000000000D5275646F6C706820436C61757300545FD51A9ACC420040
      00000000000000003EA7401147656F726765204265616E202620436F2E132337
      33204B696E672053616C6D6F6E20576179064C75676F6666024E430532393037
      380255530C3830332D3433382D323737310C3830332D3433382D333030330000
      0000000000000A42696C6C20577965727300386112AC9ACC4200400000000000
      00000050A7401950726F66657373696F6E616C204469766572732C204C74642E
      1034373334204D656C696E64612053742E06486F6F76657202414C0533323134
      350255530C3230352D3535352D383333330C3230352D3535352D343035340000
      0000000000000F536869726C6579204D617468657273001C4490F89ACC420040
      0000000000000000C2A74014446976657273206F6620426C75652D677265656E
      1036333420436F6D706C6578204176652E0650656C68616D02414C0533323134
      350255530C3230352D3535352D373138340C3230352D3535352D363035390000
      0000000000000A4E616E6379204265616E00988CDA4E9ACC4200400000000000
      000000C4A74011476F6C6420436F61737420537570706C79133232332D422048
      6F7573746F6E20506C616365064D6F62696C6502414C0533303639360255530C
      3230352D3535352D323634300C3230352D3535352D3430393400000000000000
      000C456C61696E652046616C6C7300A0F704A69BCC4200400000000000000000
      D6A7401553616E205061626C6F20446976652043656E74657211313730312D44
      204E2042726F61647761790B53616E7461204D61726961024341053935343433
      0255530C3832332D3034342D323931300C3832332D3034342D32393930000000
      0000000000105061747269636961204F27427269656E0044EFE7BD9BCC420040
      0000000000000000D8A74015556E64657277617465722053706F72747320436F
      2E123335312D412053617261736F74612053742E0853616E204A6F7365024341
      0539323139350255530C3430382D3836372D303539340C3430382D3836372D30
      30393400000000000000000C446176652057616C6C696E6700F0E9F7659CCC42
      00400000000000000000DAA74015416D65726963616E20534355424120537570
      706C7914313733392041746C616E746963204176656E7565064C6F6D69746102
      43410539313737300255530C3231332D3635342D303039320C3231332D363534
      2D303039350000000000000000104C796E6E2043696E6369726970696E69002C
      AD736C9DCC4200400000000000000000DCA74013436174616D6172616E204469
      766520436C756216426F782032363420506C65617375726520506F696E740F43
      6174616C696E612049736C616E640243410539303734300255530C3231332D32
      32332D303934310C3231332D3232332D3233323400000000000000000D4E6963
      6F6C65204475706F6E7400B06C4BA79CCC4200400000000000000000DEA7400E
      446976657227732047726F74746F14323436303120556E6976657273616C204C
      616E6506446F776E65790243410539343232300255530C3231332D3433322D30
      3039330C3231332D3433322D3438323100000000000000000A5065746572204F
      77656E00148C75089DCC42004014000000000000009EA8400F4669736865726D
      616E2773204579650B504F20426F7820373534320C4772616E64204361796D61
      6E1342726974697368205765737420496E646965730C3830392D3535352D3436
      38300C3830392D3535352D3436383900000000000000000C42657468616E204C
      657769730030BCCCD99BCC4200400400000000000000ACA84013416374696F6E
      20446976657220537570706C7910426C7565205370617220426F782023330A53
      742E2054686F6D61730530303832301155532056697267696E2049736C616E64
      730C32322D34342D3530303231310C32322D34342D3530303539360000000000
      0000000E4D617269616E6E65204D696C6573004C85DB7598CC42004014000000
      000000003EAC40134D6172696E612053435542412043656E74657216504F2042
      6F78203832343338205A756C75203738333107436172616361730956656E657A
      75656C610B35382D33332D36363232320B35382D33332D363630343900000000
      000000000E5374657068656E20427279616E7400180C805C97CC420040000000
      000000000020AF4014426C756520476C6173732048617070696E657373123633
      343520572E2053686F7265204C616E650C53616E7461204D6F6E696361024341
      0539303431300255530C3231332D3535352D313938340C3231332D3535352D31
      393935000000000000000010436872697374696E65205461796C6F7200ACFD03
      169DCC4200400000000000000000D8B04010446976657273206F662056656E69
      63650E32323020456C6D205374726565740656656E69636502464C0533393232
      340255530C3831332D3434332D323335360C3831332D3434332D393834320000
      0000000000000C53696D6F6E6520477265656E00B823820A99CC420040000000
      0000000000B3B1400F4F6E2D54617267657420534355424115372D3733373633
      204E616E616B61776120526F61640857696E6E69706567084D616E69746F6261
      074A3252203554330643616E6164610C3431362D3434352D303938380C343136
      2D3434352D3032323300000000000000000D4272616D205068696C6C69707300
      C00E81CC99CC42004010000000000000002CB240114A616D616963612053756E
      2C20496E632E0A504F20426F78203634330B52756E6177617920426179074A61
      6D616963610B5765737420496E646965730C3830392D3535352D323734360C38
      30392D3535352D3039323900000000000000000D4A6F6E617468616E20576573
      740054C4A73A9DCC42004010000000000000004CB24012556E64657277617465
      722046616E746173790A504F20426F7820383432094F63686F2052696F73074A
      616D616963610B5765737420496E646965730C3830392D3535352D323231340C
      3830392D3535352D3232333400000000000000000F4772616E742041696E7377
      6F72746800E8BD1E199CCC42004014000000000000000CB440155072696E6365
      73732049736C616E6420534355424111504F20426F7820333220576169796576
      6F0754617665756E690446696A690A3637392D3331313932330A3637392D3331
      3132303300000000000000000D416E6E65204D617269616368690034564F018B
      CC42004004000000000000001FB4401B43656E7472616C20556E646572776174
      657220537570706C6965730A504F20426F78203733370C4A6F68616E6E657362
      75726704323034321352657075626C696320536F2E204166726963610D32372D
      31312D343433323435380D32372D31312D343433333235390000000000000000
      0E4D61726961204576656E746F736800AC7945E59CCC42004014000000000000
      002BB4401453616661726920556E64657220746865205365610B504F20426F78
      20373435360C4772616E64204361796D616E1342726974697368205765737420
      496E646965730C3830392D3430392D343233330C3830392D3430392D33303032
      000000000000000009416E6E61205261636B00A8F851A398CC42004000000000
      000000002DB440154C61727279277320446976696E67205363686F6F6C143335
      3632204E5720427275636520537472656574094D696C7761756B6965024F5205
      39363237370255530C3530332D3430332D373737370C3530332D3430332D3030
      353900000000000000000E49736162656C6C65204E656563650074F660229BCC
      420040140000000000000008B5400E546F726120546F726120546F72610D504F
      20426F7820482D34353733064E617373617507426168616D61730C3830392D38
      39382D303034330C3830392D3839382D3938363400000000000000000B4B6576
      696E20526964657200989BFEBD9DCC420040000000000000000024B5400F5661
      73686F6E2056656E747572657311373433204B6579686F6C6520436F75727408
      486F6E6F6C756C750248490539323835360255530C3533322D3039392D303432
      330C3533322D3039392D3636353400000000000000000B537573616E20536D69
      746800583E61E399CC420040140000000000000038B5400F4469766572732D66
      6F722D486972650D472E4F2E205020426F7820393104537576610446696A690A
      3637392D3830343537360A3637392D30353933343500000000000000000A4A6F
      652048617474657200203117FF99CC42004000000000000000008BB540104F63
      65616E20416476656E747572657310504F20426F7820343636204B6968656904
      4D6175690248490539353733360255530C3737362D3836382D393333340C3737
      362D3836382D3935353300000000000000000A5061756C205374696C6C00B05D
      11779DCC420040040000000000000047B84018556E6465727761746572205343
      55424120436F6D70616E790C504F20426F7820536E20393108536F6D65727365
      74045358424E074265726D7564610C3830392D3535352D313232350C3830392D
      3535352D323434350000000000000000104D69636861656C2047726F73736D61
      6E0068BC22B19ACC4200400000000000000000A8B8400D417175617469632044
      72616D61123932312045766572676C61646573205761790554616D706102464C
      0533303634330255530C3631332D3434322D373635340C3631332D3434322D37
      36373800000000000000000C47696C6C69616E204F77656E007008D62F88CC42
      0040040000000000000074B9401254686520446976696E6720436F6D70616E79
      0B504F20426F7820383533350A53742E2054686F6D6173053030383230115553
      2056697267696E2049736C616E64730B32322D34342D35303039380B32322D34
      342D303938373800000000000000000B427269616E204D696C657300288AD3DC
      99CC4200400400000000000000B6B940184E6F72776573742765722053435542
      41204C696D697465640B504F20426F782036383334055061676574045053425A
      074265726D7564610C3737382D3132332D303734350C3737382D3132332D3937
      303500000000000000000C416E67656C61204A6F6E657300E8CBF6B89DCC4200
      4000000000000000009CBA4017576174657273706F7574205343554241204365
      6E7465721237383635204E45204261726265722043742E08506F72746C616E64
      024F520539393237310255530C3530332D3635342D323433340C3530332D3635
      342D3939383600000000000000000E5269636861726420486F7573657200048A
      36919CCC420040100000000000008038C340184E657074756E65277320547269
      64656E7420537570706C790A504F20426F7820313239064E656772696C074A61
      6D616963610B5765737420496E646965730C3737382D3839372D333534360C37
      37382D3839372D3636343300000000000000000D4C6F75697365204672616E6B
      7300446FBD72A0CC42}
    Active = True
    Aggregates = <>
    FieldOptions.AutoCreateMode = acCombineAlways
    FieldOptions.PositionMode = poFirst
    FieldOptions.UpdatePersistent = True
    Params = <>
    Left = 168
    Top = 152
    object QuCustomerFullAddress: TStringField
      DisplayWidth = 256
      FieldKind = fkCalculated
      FieldName = 'FullAddress'
      Size = 256
      Calculated = True
    end
  end
end
