//***************************************************
//
//    This is a VERY, VERY buggy implementation!!!
//
//****************************************************

unit TaskSchd;

//***************************************************
//  Microsoft Windows TaskScheduler API v.2.0
//****************************************************

interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

const
  // TypeLibrary Major and minor versions
  TaskSchedulerMajorVersion = 2;
  TaskSchedulerMinorVersion = 0;

  LIBID_TaskScheduler: TGUID = '{E34CB9F1-C7F7-424C-BE29-027DCC09363A}';

  IID_ITaskFolderCollection: TGUID = '{79184A66-8664-423F-97F1-637356A5D812}';
  IID_ITaskFolder: TGUID = '{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}';
  IID_IRegisteredTask: TGUID = '{9C86F320-DEE3-4DD1-B972-A303F26B061E}';
  IID_IRunningTask: TGUID = '{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}';
  IID_IRunningTaskCollection: TGUID = '{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}';
  IID_ITaskDefinition: TGUID = '{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}';
  IID_IRegistrationInfo: TGUID = '{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}';
  IID_ITriggerCollection: TGUID = '{85DF5081-1B24-4F32-878A-D9D14DF4CB77}';
  IID_ITrigger: TGUID = '{09941815-EA89-4B5B-89E0-2A773801FAC3}';
  IID_IRepetitionPattern: TGUID = '{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}';
  IID_ITaskSettings: TGUID = '{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}';
  IID_IIdleSettings: TGUID = '{84594461-0053-4342-A8FD-088FABF11F32}';
  IID_INetworkSettings: TGUID = '{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}';
  IID_IPrincipal: TGUID = '{D98D51E5-C9B4-496A-A9C1-18980261CF0F}';
  IID_IActionCollection: TGUID = '{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}';
  IID_IAction: TGUID = '{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}';
  IID_IRegisteredTaskCollection: TGUID = '{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}';
  IID_ITaskService: TGUID = '{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}';
  IID_ITaskHandler: TGUID = '{839D7762-5121-4009-9234-4F0D19394F04}';
  IID_ITaskHandlerStatus: TGUID = '{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}';
  IID_ITaskVariables: TGUID = '{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}';
  IID_ITaskNamedValuePair: TGUID = '{39038068-2B46-4AFD-8662-7BB6F868D221}';
  IID_ITaskNamedValueCollection: TGUID = '{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}';
  IID_IIdleTrigger: TGUID = '{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}';
  IID_ILogonTrigger: TGUID = '{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}';
  IID_ISessionStateChangeTrigger: TGUID = '{754DA71B-4385-4475-9DD9-598294FA3641}';
  IID_IEventTrigger: TGUID = '{D45B0167-9653-4EEF-B94F-0732CA7AF251}';
  IID_ITimeTrigger: TGUID = '{B45747E0-EBA7-4276-9F29-85C5BB300006}';
  IID_IDailyTrigger: TGUID = '{126C5CD8-B288-41D5-8DBF-E491446ADC5C}';
  IID_IWeeklyTrigger: TGUID = '{5038FC98-82FF-436D-8728-A512A57C9DC1}';
  IID_IMonthlyTrigger: TGUID = '{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}';
  IID_IMonthlyDOWTrigger: TGUID = '{77D025A3-90FA-43AA-B52E-CDA5499B946A}';
  IID_IBootTrigger: TGUID = '{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}';
  IID_IRegistrationTrigger: TGUID = '{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}';
  IID_IExecAction: TGUID = '{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}';
  IID_IShowMessageAction: TGUID = '{505E9E68-AF89-46B8-A30F-56162A83D537}';
  IID_IComHandlerAction: TGUID = '{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}';
  IID_IEmailAction: TGUID = '{10F62C64-7E16-4314-A0C2-0C3683F99D40}';
  IID_ITaskSettings2: TGUID = '{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}';

  CLSID_TaskScheduler: TGUID = '{0F87369F-A4E5-4CFC-BD3E-73E6154572DD}';
  CLSID_TaskHandlerPS: TGUID = '{F2A69DB7-DA2C-4352-9066-86FEE6DACAC9}';
  CLSID_TaskHandlerStatusPS: TGUID = '{9F15266D-D7BA-48F0-93C1-E6895F6FE5AC}';

type
  TTaskRunFlags = (
    TASK_RUN_NO_FLAGS	= $0,
    TASK_RUN_AS_SELF	= $1,
    TASK_RUN_IGNORE_CONSTRAINTS	= $2,
    TASK_RUN_USE_SESSION_ID	= $4,
    TASK_RUN_USER_SID	= $8
  );

  TASK_ENUM_FLAGS = (
    TASK_ENUM_HIDDEN = $1
  );

  TASK_LOGON_TYPE = (
    TASK_LOGON_NONE = 0,
    TASK_LOGON_PASSWORD = 1,
    TASK_LOGON_S4U = 2,
    TASK_LOGON_INTERACTIVE_TOKEN = 3,
    TASK_LOGON_GROUP = 4,
    TASK_LOGON_SERVICE_ACCOUNT = 5,
    TASK_LOGON_INTERACTIVE_TOKEN_OR_PASSWORD = 6
  );

  TASK_RUNLEVEL_TYPE = (
    TASK_RUNLEVEL_LUA	= 0,
    TASK_RUNLEVEL_HIGHEST	= 1
  );

  TASK_STATE = (
    TASK_STATE_UNKNOWN = 0,
    TASK_STATE_DISABLED	= 1,
    TASK_STATE_QUEUED	= 2,
    TASK_STATE_READY	= 3,
    TASK_STATE_RUNNING	= 4
  );

  TASK_CREATION = (
    TASK_VALIDATE_ONLY = $1,
    TASK_CREATE	= $2,
    TASK_UPDATE	= $4,
    TASK_CREATE_OR_UPDATE	= $6,
    TASK_DISABLE	= $8,
    TASK_DONT_ADD_PRINCIPAL_ACE	= $10,
    TASK_IGNORE_REGISTRATION_TRIGGERS	= $20
  );

type
  TASK_TRIGGER_TYPE2 = (
    TASK_TRIGGER_EVENT	= 0,
    TASK_TRIGGER_TIME	= 1,
    TASK_TRIGGER_DAILY	= 2,
    TASK_TRIGGER_WEEKLY	= 3,
    TASK_TRIGGER_MONTHLY	= 4,
    TASK_TRIGGER_MONTHLYDOW	= 5,
    TASK_TRIGGER_IDLE	= 6,
    TASK_TRIGGER_REGISTRATION	= 7,
    TASK_TRIGGER_BOOT	= 8,
    TASK_TRIGGER_LOGON	= 9,
    TASK_TRIGGER_SESSION_STATE_CHANGE	= 11
  );

  TASK_SESSION_STATE_CHANGE_TYPE = (
    TASK_CONSOLE_CONNECT	= 1,
    TASK_CONSOLE_DISCONNECT	= 2,
    TASK_REMOTE_CONNECT	= 3,
    TASK_REMOTE_DISCONNECT	= 4,
    TASK_SESSION_LOCK	= 7,
    TASK_SESSION_UNLOCK	= 8
  );

  TASK_ACTION_TYPE = (
    TASK_ACTION_EXEC = 0,
    TASK_ACTION_COM_HANDLER	= 5,
    TASK_ACTION_SEND_EMAIL	= 6,
    TASK_ACTION_SHOW_MESSAGE	= 7
  );

  TASK_INSTANCES_POLICY = (
    TASK_INSTANCES_PARALLEL	= 0,
    TASK_INSTANCES_QUEUE	= 1,
    TASK_INSTANCES_IGNORE_NEW	= 2,
    TASK_INSTANCES_STOP_EXISTING	= 3
  );

  TASK_COMPATIBILITY = (
    TASK_COMPATIBILITY_AT	= 0,
    TASK_COMPATIBILITY_V1	= 1,
    TASK_COMPATIBILITY_V2	= 2
  );

type
    ITaskFolderCollection = interface;
    ITaskFolder = interface;
    IRegisteredTask = interface;
    IRunningTask = interface;
    IRunningTaskCollection = interface;
    ITaskDefinition = interface;
    IRegistrationInfo = interface;


    IRepetitionPattern = interface;
    ITaskSettings = interface;
    IIdleSettings = interface;
    INetworkSettings = interface;
    IPrincipal = interface;
    IRegisteredTaskCollection = interface;
    ITaskService = interface;
    ITaskHandler = interface;
    ITaskHandlerStatus = interface;
    ITaskVariables = interface;
    ITaskNamedValuePair = interface;
    ITaskNamedValueCollection = interface;

    ITriggerCollection = interface;
    ITrigger = interface;
    IIdleTrigger = interface;
    ILogonTrigger = interface;
    ISessionStateChangeTrigger = interface;
    IEventTrigger = interface;
    ITimeTrigger = interface;
    IDailyTrigger = interface;
    IWeeklyTrigger = interface;
    IMonthlyTrigger = interface;
    IMonthlyDOWTrigger = interface;
    IBootTrigger = interface;
    IRegistrationTrigger = interface;


    IActionCollection = interface;
    IAction = interface;
    IExecAction = interface;
    IShowMessageAction = interface;
    IComHandlerAction = interface;
    IEmailAction = interface;

    ITaskSettings2 = interface;


// *********************************************************************//
// Interface: ITaskFolderCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79184A66-8664-423F-97F1-637356A5D812}
// *********************************************************************//
  ITaskFolderCollection = interface(IDispatch)
    ['{79184A66-8664-423F-97F1-637356A5D812}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): ITaskFolder; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: ITaskFolder read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;



// *********************************************************************//
// Interface: ITaskFolder
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}
// *********************************************************************//
  ITaskFolder = interface(IDispatch)
    ['{8CFAC062-A080-4C15-9A88-AA7C2AF80DFC}']
    function Get_Name: TBStr; safecall;
    function Get_Path: TBStr; safecall;
    function GetFolder(Path: TBStr; out ppFolder: ITaskFolder): HRESULT; stdcall;
    function GetFolders(flags: LONG): ITaskFolderCollection; stdcall;
    function CreateFolder(subFolderName: TBStr; sddl: OleVariant; out ppFolder: ITaskFolder): HRESULT; stdcall;
    procedure DeleteFolder(subFolderName: TBStr; flags: LONG); stdcall;
    function GetTask(path: TBStr; out ppTask: IRegisteredTask): HRESULT; stdcall;
    function GetTasks(flags: LONG; out ppTasks: IRegisteredTaskCollection): HRESULT; stdcall;
    procedure DeleteTask(Name: TBStr; flags: LONG); stdcall;
    function RegisterTask(Path: TBStr; XmlText: TBStr; flags: LONG;
                          UserId: OleVariant; password: OleVariant; LogonType: TASK_LOGON_TYPE;
                          sddl: OleVariant; out ppTask: IRegisteredTask): HRESULT; stdcall;
    function RegisterTaskDefinition(path: TBStr; pDefinition: ITaskDefinition;
                                    flags: LONG; TASK_CREATION: OleVariant; password: OleVariant;
                                    logonType: TASK_LOGON_TYPE; sddl: OleVariant;
                                    out ppTask: IRegisteredTask): HRESULT; stdcall;
    function GetSecurityDescriptor(securityInformation: Integer): TBStr; stdcall;
    procedure SetSecurityDescriptor(sddl: TBStr; flags: LONG); stdcall;

    property Name: TBStr read Get_Name;
    property Path: TBStr read Get_Path;
  end;



// *********************************************************************//
// Interface: IRegisteredTask
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9C86F320-DEE3-4DD1-B972-A303F26B061E}
// *********************************************************************//
  IRegisteredTask = interface(IDispatch)
    ['{9C86F320-DEE3-4DD1-B972-A303F26B061E}']
    function Get_Name: TBStr; safecall;
    function Get_Path: TBStr; safecall;
    function Get_State: TASK_STATE; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function Run(params: OleVariant): IRunningTask; stdcall;
    function RunEx(params: OleVariant; flags: LONG; sessionID: Integer; user: TBStr): IRunningTask; stdcall;
    function GetInstances(flags: LONG): IRunningTaskCollection; stdcall;
    function Get_LastRunTime: TDateTime; safecall;
    function Get_LastTaskResult: Integer; safecall;
    function Get_NumberOfMissedRuns: Integer; safecall;
    function Get_NextRunTime: TDateTime; safecall;
    function Get_Definition: ITaskDefinition; safecall;
    function Get_Xml: TBStr; safecall;
    function GetSecurityDescriptor(securityInformation: Integer): TBStr; stdcall;
    procedure SetSecurityDescriptor(sddl: TBStr; flags: LONG); stdcall;
    procedure Stop(flags: LONG); stdcall;
    procedure GetRunTimes(var pstStart: _SYSTEMTIME; var pstEnd: TSystemTime; var pCount: LongWord;
                          out pRunTimes: PSystemTime); stdcall;       // pSystemTime ?

    property Name: TBStr read Get_Name;
    property Path: TBStr read Get_Path;
    property State: TASK_STATE read Get_State;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property LastRunTime: TDateTime read Get_LastRunTime;
    property LastTaskResult: Integer read Get_LastTaskResult;
    property NumberOfMissedRuns: Integer read Get_NumberOfMissedRuns;
    property NextRunTime: TDateTime read Get_NextRunTime;
    property Definition: ITaskDefinition read Get_Definition;
    property Xml: TBStr read Get_Xml;
  end;


// *********************************************************************//
// Interface: IRunningTask
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {653758FB-7B9A-4F1E-A471-BEEB8E9B834E}
// *********************************************************************//
  IRunningTask = interface(IDispatch)
    ['{653758FB-7B9A-4F1E-A471-BEEB8E9B834E}']
    function Get_Name: TBStr; safecall;
    function Get_InstanceGuid: TBStr; safecall;
    function Get_Path: TBStr; safecall;
    function Get_State: TASK_STATE; safecall;
    function Get_CurrentAction: TBStr; safecall;
    procedure Stop; stdcall;
    procedure Refresh; stdcall;
    function Get_EnginePID: LongWord; safecall;
    property Name: TBStr read Get_Name;
    property InstanceGuid: TBStr read Get_InstanceGuid;
    property Path: TBStr read Get_Path;
    property State: TASK_STATE read Get_State;
    property CurrentAction: TBStr read Get_CurrentAction;
    property EnginePID: LongWord read Get_EnginePID;
  end;



// *********************************************************************//
// Interface: IRunningTaskCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}
// *********************************************************************//
  IRunningTaskCollection = interface(IDispatch)
    ['{6A67614B-6828-4FEC-AA54-6D52E8F1F2DB}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): IRunningTask; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: IRunningTask read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;



// *********************************************************************//
// Interface: ITaskDefinition
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}
// *********************************************************************//
  ITaskDefinition = interface(IDispatch)
    ['{F5BC8FC5-536D-4F77-B852-FBC1356FDEB6}']
    function Get_RegistrationInfo: IRegistrationInfo; safecall;
    procedure Set_RegistrationInfo(ppRegistrationInfo: IRegistrationInfo); safecall;
    function Get_Triggers: ITriggerCollection; safecall;
    procedure Set_Triggers(ppTriggers: ITriggerCollection); safecall;
    function Get_Settings: ITaskSettings; safecall;
    procedure Set_Settings(ppSettings: ITaskSettings); safecall;
    function Get_Data: TBStr; safecall;
    procedure Set_Data(pData: TBStr); stdcall;
    function Get_Principal: IPrincipal; safecall;
    procedure Set_Principal(ppPrincipal: IPrincipal); safecall;
    function Get_Actions: IActionCollection; safecall;
    procedure Set_Actions(ppActions: IActionCollection); safecall;
    function Get_XmlText: TBStr; safecall;
    procedure Set_XmlText(pXml: TBStr); safecall;
    property RegistrationInfo: IRegistrationInfo read Get_RegistrationInfo write Set_RegistrationInfo;
    property Triggers: ITriggerCollection read Get_Triggers write Set_Triggers;
    property Settings: ITaskSettings read Get_Settings write Set_Settings;
    property Data: TBStr read Get_Data write Set_Data;
    property Principal: IPrincipal read Get_Principal write Set_Principal;
    property Actions: IActionCollection read Get_Actions write Set_Actions;
    property XmlText: TBStr read Get_XmlText write Set_XmlText;
  end;

// *********************************************************************//
// Interface: IRegistrationInfo
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}
// *********************************************************************//
  IRegistrationInfo = interface(IDispatch)
    ['{416D8B73-CB41-4EA1-805C-9BE9A5AC4A74}']
    function Get_Description: TBStr; safecall;
    procedure Set_Description(pDescription: TBStr); safecall;
    function Get_Author: TBStr; safecall;
    procedure Set_Author(pAuthor: TBStr); safecall;
    function Get_Version: TBStr; safecall;
    procedure Set_Version(pVersion: TBStr); safecall;
    function Get_Date: TBStr; safecall;
    procedure Set_Date(pDate: TBStr); safecall;
    function Get_Documentation: TBStr; safecall;
    procedure Set_Documentation(pDocumentation: TBStr); safecall;
    function Get_XmlText: TBStr; safecall;
    procedure Set_XmlText(pText: TBStr); safecall;
    function Get_URI: TBStr; safecall;
    procedure Set_URI(pUri: TBStr); safecall;
    function Get_SecurityDescriptor: OleVariant; safecall;
    procedure Set_SecurityDescriptor(pSddl: OleVariant); safecall;
    function Get_Source: TBStr; safecall;
    procedure Set_Source(pSource: TBStr); safecall;
    property Description: TBStr read Get_Description write Set_Description;
    property Author: TBStr read Get_Author write Set_Author;
    property Version: TBStr read Get_Version write Set_Version;
    property Date: TBStr read Get_Date write Set_Date;
    property Documentation: TBStr read Get_Documentation write Set_Documentation;
    property XmlText: TBStr read Get_XmlText write Set_XmlText;
    property URI: TBStr read Get_URI write Set_URI;
    property SecurityDescriptor: OleVariant read Get_SecurityDescriptor write Set_SecurityDescriptor;
    property Source: TBStr read Get_Source write Set_Source;
  end;


// *********************************************************************//
// Interface: ITriggerCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {85DF5081-1B24-4F32-878A-D9D14DF4CB77}
// *********************************************************************//
  ITriggerCollection = interface(IDispatch)
    ['{85DF5081-1B24-4F32-878A-D9D14DF4CB77}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): ITrigger; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Create(triggerType: TASK_TRIGGER_TYPE2): ITrigger; stdcall;
    procedure Remove(index: OleVariant); stdcall;
    procedure Clear; stdcall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: ITrigger read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// Interface: ITrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {09941815-EA89-4B5B-89E0-2A773801FAC3}
// *********************************************************************//
  ITrigger = interface(IDispatch)
    ['{09941815-EA89-4B5B-89E0-2A773801FAC3}']
    function Get_type: TASK_TRIGGER_TYPE2; safecall;
    function Get_Id: TBStr; safecall;
    procedure Set_Id(pId: TBStr); safecall;
    function Get_Repetition: IRepetitionPattern; safecall;
    procedure Set_Repetition(ppRepeat: IRepetitionPattern); safecall;
    function Get_ExecutionTimeLimit: TBStr; safecall;
    procedure Set_ExecutionTimeLimit(pTimeLimit: TBStr); safecall;
    function Get_StartBoundary: TBStr; safecall;
    procedure Set_StartBoundary(pStart: TBStr); safecall;
    function Get_EndBoundary: TBStr; safecall;
    procedure Set_EndBoundary(pEnd: TBStr); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    property triggerType: TASK_TRIGGER_TYPE2 read Get_type;
    property Id: TBStr read Get_Id write Set_Id;
    property Repetition: IRepetitionPattern read Get_Repetition write Set_Repetition;
    property ExecutionTimeLimit: TBStr read Get_ExecutionTimeLimit write Set_ExecutionTimeLimit;
    property StartBoundary: TBStr read Get_StartBoundary write Set_StartBoundary;
    property EndBoundary: TBStr read Get_EndBoundary write Set_EndBoundary;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
  end;

// *********************************************************************//
// Interface: IRepetitionPattern
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}
// *********************************************************************//
  IRepetitionPattern = interface(IDispatch)
    ['{7FB9ACF1-26BE-400E-85B5-294B9C75DFD6}']
    function Get_Interval: TBStr; safecall;
    procedure Set_Interval(pInterval: TBStr); safecall;
    function Get_Duration: TBStr; safecall;
    procedure Set_Duration(pDuration: TBStr); safecall;
    function Get_StopAtDurationEnd: WordBool; safecall;
    procedure Set_StopAtDurationEnd(pStop: WordBool); safecall;
    property Interval: TBStr read Get_Interval write Set_Interval;
    property Duration: TBStr read Get_Duration write Set_Duration;
    property StopAtDurationEnd: WordBool read Get_StopAtDurationEnd write Set_StopAtDurationEnd;
  end;


// *********************************************************************//
// Interface: ITaskSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {8FD4711D-2D02-4C8C-87E3-EFF699DE127E}
// *********************************************************************//
  ITaskSettings = interface(IDispatch)
    ['{8FD4711D-2D02-4C8C-87E3-EFF699DE127E}']
    function Get_AllowDemandStart: WordBool; safecall;
    procedure Set_AllowDemandStart(pAllowDemandStart: WordBool); safecall;
    function Get_RestartInterval: TBStr; safecall;
    procedure Set_RestartInterval(pRestartInterval: TBStr); safecall;
    function Get_RestartCount: SYSINT; safecall;
    procedure Set_RestartCount(pRestartCount: SYSINT); safecall;
    function Get_MultipleInstances: TASK_INSTANCES_POLICY; safecall;
    procedure Set_MultipleInstances(pPolicy: TASK_INSTANCES_POLICY); safecall;
    function Get_StopIfGoingOnBatteries: WordBool; safecall;
    procedure Set_StopIfGoingOnBatteries(pStopIfOnBatteries: WordBool); safecall;
    function Get_DisallowStartIfOnBatteries: WordBool; safecall;
    procedure Set_DisallowStartIfOnBatteries(pDisallowStart: WordBool); safecall;
    function Get_AllowHardTerminate: WordBool; safecall;
    procedure Set_AllowHardTerminate(pAllowHardTerminate: WordBool); safecall;
    function Get_StartWhenAvailable: WordBool; safecall;
    procedure Set_StartWhenAvailable(pStartWhenAvailable: WordBool); safecall;
    function Get_XmlText: TBStr; safecall;
    procedure Set_XmlText(pText: TBStr); safecall;
    function Get_RunOnlyIfNetworkAvailable: WordBool; safecall;
    procedure Set_RunOnlyIfNetworkAvailable(pRunOnlyIfNetworkAvailable: WordBool); safecall;
    function Get_ExecutionTimeLimit: TBStr; safecall;
    procedure Set_ExecutionTimeLimit(pExecutionTimeLimit: TBStr); safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(pEnabled: WordBool); safecall;
    function Get_DeleteExpiredTaskAfter: TBStr; safecall;
    procedure Set_DeleteExpiredTaskAfter(pExpirationDelay: TBStr); safecall;
    function Get_Priority: SYSINT; safecall;
    procedure Set_Priority(pPriority: SYSINT); safecall;
    function Get_Compatibility: TASK_COMPATIBILITY; safecall;
    procedure Set_Compatibility(pCompatLevel: TASK_COMPATIBILITY); safecall;
    function Get_Hidden: WordBool; safecall;
    procedure Set_Hidden(pHidden: WordBool); safecall;
    function Get_IdleSettings: IIdleSettings; safecall;
    procedure Set_IdleSettings(ppIdleSettings: IIdleSettings); safecall;
    function Get_RunOnlyIfIdle: WordBool; safecall;
    procedure Set_RunOnlyIfIdle(pRunOnlyIfIdle: WordBool); safecall;
    function Get_WakeToRun: WordBool; safecall;
    procedure Set_WakeToRun(pWake: WordBool); safecall;
    function Get_NetworkSettings: INetworkSettings; safecall;
    procedure Set_NetworkSettings(ppNetworkSettings: INetworkSettings); safecall;
    property AllowDemandStart: WordBool read Get_AllowDemandStart write Set_AllowDemandStart;
    property RestartInterval: TBStr read Get_RestartInterval write Set_RestartInterval;
    property RestartCount: SYSINT read Get_RestartCount write Set_RestartCount;
    property MultipleInstances: TASK_INSTANCES_POLICY read Get_MultipleInstances write Set_MultipleInstances;
    property StopIfGoingOnBatteries: WordBool read Get_StopIfGoingOnBatteries write Set_StopIfGoingOnBatteries;
    property DisallowStartIfOnBatteries: WordBool read Get_DisallowStartIfOnBatteries write Set_DisallowStartIfOnBatteries;
    property AllowHardTerminate: WordBool read Get_AllowHardTerminate write Set_AllowHardTerminate;
    property StartWhenAvailable: WordBool read Get_StartWhenAvailable write Set_StartWhenAvailable;
    property XmlText: TBStr read Get_XmlText write Set_XmlText;
    property RunOnlyIfNetworkAvailable: WordBool read Get_RunOnlyIfNetworkAvailable write Set_RunOnlyIfNetworkAvailable;
    property ExecutionTimeLimit: TBStr read Get_ExecutionTimeLimit write Set_ExecutionTimeLimit;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property DeleteExpiredTaskAfter: TBStr read Get_DeleteExpiredTaskAfter write Set_DeleteExpiredTaskAfter;
    property Priority: SYSINT read Get_Priority write Set_Priority;
    property Compatibility: TASK_COMPATIBILITY read Get_Compatibility write Set_Compatibility;
    property Hidden: WordBool read Get_Hidden write Set_Hidden;
    property IdleSettings: IIdleSettings read Get_IdleSettings write Set_IdleSettings;
    property RunOnlyIfIdle: WordBool read Get_RunOnlyIfIdle write Set_RunOnlyIfIdle;
    property WakeToRun: WordBool read Get_WakeToRun write Set_WakeToRun;
    property NetworkSettings: INetworkSettings read Get_NetworkSettings write Set_NetworkSettings;
  end;


// *********************************************************************//
// Interface: IIdleSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {84594461-0053-4342-A8FD-088FABF11F32}
// *********************************************************************//
  IIdleSettings = interface(IDispatch)
    ['{84594461-0053-4342-A8FD-088FABF11F32}']
    function Get_IdleDuration: TBStr; safecall;
    procedure Set_IdleDuration(pDelay: TBStr); safecall;
    function Get_WaitTimeout: TBStr; safecall;
    procedure Set_WaitTimeout(pTimeout: TBStr); safecall;
    function Get_StopOnIdleEnd: WordBool; safecall;
    procedure Set_StopOnIdleEnd(pStop: WordBool); safecall;
    function Get_RestartOnIdle: WordBool; safecall;
    procedure Set_RestartOnIdle(pRestart: WordBool); safecall;
    property IdleDuration: TBStr read Get_IdleDuration write Set_IdleDuration;
    property WaitTimeout: TBStr read Get_WaitTimeout write Set_WaitTimeout;
    property StopOnIdleEnd: WordBool read Get_StopOnIdleEnd write Set_StopOnIdleEnd;
    property RestartOnIdle: WordBool read Get_RestartOnIdle write Set_RestartOnIdle;
  end;

// *********************************************************************//
// Interface: INetworkSettings
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9F7DEA84-C30B-4245-80B6-00E9F646F1B4}
// *********************************************************************//
  INetworkSettings = interface(IDispatch)
    ['{9F7DEA84-C30B-4245-80B6-00E9F646F1B4}']
    function Get_Name: TBStr; safecall;
    procedure Set_Name(pName: TBStr); safecall;
    function Get_Id: TBStr; safecall;
    procedure Set_Id(pId: TBStr); safecall;
    property Name: TBStr read Get_Name write Set_Name;
    property Id: TBStr read Get_Id write Set_Id;
  end;


// *********************************************************************//
// Interface: IPrincipal
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D98D51E5-C9B4-496A-A9C1-18980261CF0F}
// *********************************************************************//
  IPrincipal = interface(IDispatch)
    ['{D98D51E5-C9B4-496A-A9C1-18980261CF0F}']
    function Get_Id: TBStr; safecall;
    procedure Set_Id(pId: TBStr); safecall;
    function Get_DisplayName: TBStr; safecall;
    procedure Set_DisplayName(pName: TBStr); safecall;
    function Get_UserId: TBStr; safecall;
    procedure Set_UserId(pUser: TBStr); safecall;
    function Get_LogonType: TASK_LOGON_TYPE; safecall;
    procedure Set_LogonType(pLogon: TASK_LOGON_TYPE); safecall;
    function Get_GroupId: TBStr; safecall;
    procedure Set_GroupId(pGroup: TBStr); safecall;
    function Get_RunLevel: TASK_RUNLEVEL_TYPE; safecall;
    procedure Set_RunLevel(pRunLevel: TASK_RUNLEVEL_TYPE); safecall;
    property Id: TBStr read Get_Id write Set_Id;
    property DisplayName: TBStr read Get_DisplayName write Set_DisplayName;
    property UserId: TBStr read Get_UserId write Set_UserId;
    property LogonType: TASK_LOGON_TYPE read Get_LogonType write Set_LogonType;
    property GroupId: TBStr read Get_GroupId write Set_GroupId;
    property RunLevel: TASK_RUNLEVEL_TYPE read Get_RunLevel write Set_RunLevel;
  end;

// *********************************************************************//
// Interface: IActionCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}
// *********************************************************************//
  IActionCollection = interface(IDispatch)
    ['{02820E19-7B98-4ED2-B2E8-FDCCCEFF619B}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): IAction; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_XmlText: TBStr; safecall;
    procedure Set_XmlText(pText: TBStr); safecall;
    function Create(ActionType: TASK_ACTION_TYPE; out ppAction: IAction): HRESULT; stdcall;
    procedure Remove(index: OleVariant); stdcall;
    procedure Clear; stdcall;
    function Get_Context: TBStr; safecall;
    procedure Set_Context(pContext: TBStr); safecall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: IAction read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
    property XmlText: TBStr read Get_XmlText write Set_XmlText;
    property Context: TBStr read Get_Context write Set_Context;
  end;

// *********************************************************************//
// Interface: IAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {BAE54997-48B1-4CBE-9965-D6BE263EBEA4}
// *********************************************************************//
  IAction = interface(IDispatch)
    ['{BAE54997-48B1-4CBE-9965-D6BE263EBEA4}']
    function Get_Id: TBStr; safecall;
    procedure Set_Id(pId: TBStr); safecall;
    function Get_type: TASK_ACTION_TYPE; safecall;
    property Id: TBStr read Get_Id write Set_Id;
    property ActionType: TASK_ACTION_TYPE read Get_type;
  end;

// *********************************************************************//
// Interface: IRegisteredTaskCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {86627EB4-42A7-41E4-A4D9-AC33A72F2D52}
// *********************************************************************//
  IRegisteredTaskCollection = interface(IDispatch)
    ['{86627EB4-42A7-41E4-A4D9-AC33A72F2D52}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: OleVariant): IRegisteredTask; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property Item[index: OleVariant]: IRegisteredTask read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// Interface: ITaskService
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2FABA4C7-4DA9-4013-9697-20CC3FD40F85}
// *********************************************************************//
  ITaskService = interface(IDispatch)
    ['{2FABA4C7-4DA9-4013-9697-20CC3FD40F85}']
    function GetFolder(Path: TBStr; out ppFolder: ITaskFolder): HRESULT; stdcall;
    function GetRunningTasks(flags: LONG): IRunningTaskCollection; stdcall;
    function NewTask(flags: DWORD; out ppDefinition: ITaskDefinition): HRESULT; stdcall;
    function Connect(serverName: OleVariant; user: OleVariant; domain: OleVariant;
                      password: OleVariant):integer; stdcall;
    function Get_Connected: WordBool; safecall;
    function Get_TargetServer: TBStr; safecall;
    function Get_ConnectedUser: TBStr; safecall;
    function Get_ConnectedDomain: TBStr; safecall;
    function Get_HighestVersion: LongWord; safecall;

    property Connected: WordBool read Get_Connected;
    property TargetServer: TBStr read Get_TargetServer;
    property ConnectedUser: TBStr read Get_ConnectedUser;
    property ConnectedDomain: TBStr read Get_ConnectedDomain;
    property HighestVersion: LongWord read Get_HighestVersion;
  end;


// *********************************************************************//
// Interface: ITaskHandler
// Flags:     (0)
// GUID:      {839D7762-5121-4009-9234-4F0D19394F04}
// *********************************************************************//
  ITaskHandler = interface(IUnknown)
    ['{839D7762-5121-4009-9234-4F0D19394F04}']
    function Start(pHandlerServices: IUnknown; Data: TBStr): HResult; stdcall;
    function Stop(out pRetCode: HResult): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskHandlerStatus
// Flags:     (0)
// GUID:      {EAEC7A8F-27A0-4DDC-8675-14726A01A38A}
// *********************************************************************//
  ITaskHandlerStatus = interface(IUnknown)
    ['{EAEC7A8F-27A0-4DDC-8675-14726A01A38A}']
    function UpdateStatus(percentComplete: Smallint; statusMessage: TBStr): HResult; stdcall;
    function TaskCompleted(taskErrCode: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskVariables
// Flags:     (0)
// GUID:      {3E4C9351-D966-4B8B-BB87-CEBA68BB0107}
// *********************************************************************//
  ITaskVariables = interface(IUnknown)
    ['{3E4C9351-D966-4B8B-BB87-CEBA68BB0107}']
    function GetInput(out pInput: TBStr): HResult; stdcall;
    function SetOutput(input: TBStr): HResult; stdcall;
    function GetContext(out pContext: TBStr): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITaskNamedValuePair
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {39038068-2B46-4AFD-8662-7BB6F868D221}
// *********************************************************************//
  ITaskNamedValuePair = interface(IDispatch)
    ['{39038068-2B46-4AFD-8662-7BB6F868D221}']
    function Get_Name: TBStr; safecall;
    procedure Set_Name(pName: TBStr); safecall;
    function Get_Value: TBStr; safecall;
    procedure Set_Value(pValue: TBStr); safecall;
    property Name: TBStr read Get_Name write Set_Name;
    property Value: TBStr read Get_Value write Set_Value;
  end;


// *********************************************************************//
// Interface: ITaskNamedValueCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}
// *********************************************************************//
  ITaskNamedValueCollection = interface(IDispatch)
    ['{B4EF826B-63C3-46E4-A504-EF69E4F7EA4D}']
    function Get_Count: Integer; safecall;
    function Get_Item(index: Integer): ITaskNamedValuePair; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Create(Name: TBStr; Value: TBStr): ITaskNamedValuePair; stdcall;
    procedure Remove(index: Integer); stdcall;
    procedure Clear; stdcall;
    property Count: Integer read Get_Count;
    property Item[index: Integer]: ITaskNamedValuePair read Get_Item; default;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// Interface: IIdleTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}
// *********************************************************************//
  IIdleTrigger = interface(ITrigger)
    ['{D537D2B0-9FB3-4D34-9739-1FF5CE7B1EF3}']
  end;


// *********************************************************************//
// Interface: ILogonTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}
// *********************************************************************//
  ILogonTrigger = interface(ITrigger)
    ['{72DADE38-FAE4-4B3E-BAF4-5D009AF02B1C}']
    function Get_Delay: TBStr; safecall;
    procedure Set_Delay(pDelay: TBStr); safecall;
    function Get_UserId: TBStr; safecall;
    procedure Set_UserId(pUser: TBStr); safecall;
    property Delay: TBStr read Get_Delay write Set_Delay;
    property UserId: TBStr read Get_UserId write Set_UserId;
  end;

// *********************************************************************//
// Interface: ISessionStateChangeTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {754DA71B-4385-4475-9DD9-598294FA3641}
// *********************************************************************//
  ISessionStateChangeTrigger = interface(ITrigger)
    ['{754DA71B-4385-4475-9DD9-598294FA3641}']
    function Get_Delay: TBStr; safecall;
    procedure Set_Delay(pDelay: TBStr); safecall;
    function Get_UserId: TBStr; safecall;
    procedure Set_UserId(pUser: TBStr); safecall;
    function Get_StateChange: TASK_SESSION_STATE_CHANGE_TYPE; safecall;
    procedure Set_StateChange(pType: TASK_SESSION_STATE_CHANGE_TYPE); safecall;
    property Delay: TBStr read Get_Delay write Set_Delay;
    property UserId: TBStr read Get_UserId write Set_UserId;
    property StateChange: TASK_SESSION_STATE_CHANGE_TYPE read Get_StateChange write Set_StateChange;
  end;


// *********************************************************************//
// Interface: IEventTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D45B0167-9653-4EEF-B94F-0732CA7AF251}
// *********************************************************************//
  IEventTrigger = interface(ITrigger)
    ['{D45B0167-9653-4EEF-B94F-0732CA7AF251}']
    function Get_Subscription: TBStr; safecall;
    procedure Set_Subscription(pQuery: TBStr); safecall;
    function Get_Delay: TBStr; safecall;
    procedure Set_Delay(pDelay: TBStr); safecall;
    function Get_ValueQueries: ITaskNamedValueCollection; safecall;
    procedure Set_ValueQueries(ppNamedXPaths: ITaskNamedValueCollection); safecall;
    property Subscription: TBStr read Get_Subscription write Set_Subscription;
    property Delay: TBStr read Get_Delay write Set_Delay;
    property ValueQueries: ITaskNamedValueCollection read Get_ValueQueries write Set_ValueQueries;
  end;

// *********************************************************************//
// Interface: ITimeTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B45747E0-EBA7-4276-9F29-85C5BB300006}
// *********************************************************************//
  ITimeTrigger = interface(ITrigger)
    ['{B45747E0-EBA7-4276-9F29-85C5BB300006}']
    function Get_RandomDelay: TBStr; safecall;
    procedure Set_RandomDelay(pRandomDelay: TBStr); safecall;
    property RandomDelay: TBStr read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// Interface: IDailyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {126C5CD8-B288-41D5-8DBF-E491446ADC5C}
// *********************************************************************//
  IDailyTrigger = interface(ITrigger)
    ['{126C5CD8-B288-41D5-8DBF-E491446ADC5C}']
    function Get_DaysInterval: Smallint; safecall;
    procedure Set_DaysInterval(pDays: Smallint); safecall;
    function Get_RandomDelay: TBStr; safecall;
    procedure Set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysInterval: Smallint read Get_DaysInterval write Set_DaysInterval;
    property RandomDelay: TBStr read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// Interface: IWeeklyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5038FC98-82FF-436D-8728-A512A57C9DC1}
// *********************************************************************//
  IWeeklyTrigger = interface(ITrigger)
    ['{5038FC98-82FF-436D-8728-A512A57C9DC1}']
    function Get_DaysOfWeek: Smallint; safecall;
    procedure Set_DaysOfWeek(pDays: Smallint); safecall;
    function Get_WeeksInterval: Smallint; safecall;
    procedure Set_WeeksInterval(pWeeks: Smallint); safecall;
    function Get_RandomDelay: TBStr; safecall;
    procedure Set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfWeek: Smallint read Get_DaysOfWeek write Set_DaysOfWeek;
    property WeeksInterval: Smallint read Get_WeeksInterval write Set_WeeksInterval;
    property RandomDelay: TBStr read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// Interface: IMonthlyTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}
// *********************************************************************//
  IMonthlyTrigger = interface(ITrigger)
    ['{97C45EF1-6B02-4A1A-9C0E-1EBFBA1500AC}']
    function Get_DaysOfMonth: Integer; safecall;
    procedure Set_DaysOfMonth(pDays: Integer); safecall;
    function Get_MonthsOfYear: Smallint; safecall;
    procedure Set_MonthsOfYear(pMonths: Smallint); safecall;
    function Get_RunOnLastDayOfMonth: WordBool; safecall;
    procedure Set_RunOnLastDayOfMonth(pLastDay: WordBool); safecall;
    function Get_RandomDelay: TBStr; safecall;
    procedure Set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfMonth: Integer read Get_DaysOfMonth write Set_DaysOfMonth;
    property MonthsOfYear: Smallint read Get_MonthsOfYear write Set_MonthsOfYear;
    property RunOnLastDayOfMonth: WordBool read Get_RunOnLastDayOfMonth write Set_RunOnLastDayOfMonth;
    property RandomDelay: TBStr read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// Interface: IMonthlyDOWTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {77D025A3-90FA-43AA-B52E-CDA5499B946A}
// *********************************************************************//
  IMonthlyDOWTrigger = interface(ITrigger)
    ['{77D025A3-90FA-43AA-B52E-CDA5499B946A}']
    function Get_DaysOfWeek: Smallint; safecall;
    procedure Set_DaysOfWeek(pDays: Smallint); safecall;
    function Get_WeeksOfMonth: Smallint; safecall;
    procedure Set_WeeksOfMonth(pWeeks: Smallint); safecall;
    function Get_MonthsOfYear: Smallint; safecall;
    procedure Set_MonthsOfYear(pMonths: Smallint); safecall;
    function Get_RunOnLastWeekOfMonth: WordBool; safecall;
    procedure Set_RunOnLastWeekOfMonth(pLastWeek: WordBool); safecall;
    function Get_RandomDelay: TBStr; safecall;
    procedure Set_RandomDelay(pRandomDelay: TBStr); safecall;
    property DaysOfWeek: Smallint read Get_DaysOfWeek write Set_DaysOfWeek;
    property WeeksOfMonth: Smallint read Get_WeeksOfMonth write Set_WeeksOfMonth;
    property MonthsOfYear: Smallint read Get_MonthsOfYear write Set_MonthsOfYear;
    property RunOnLastWeekOfMonth: WordBool read Get_RunOnLastWeekOfMonth write Set_RunOnLastWeekOfMonth;
    property RandomDelay: TBStr read Get_RandomDelay write Set_RandomDelay;
  end;

// *********************************************************************//
// Interface: IBootTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}
// *********************************************************************//
  IBootTrigger = interface(ITrigger)
    ['{2A9C35DA-D357-41F4-BBC1-207AC1B1F3CB}']
    function Get_Delay: TBStr; safecall;
    procedure Set_Delay(pDelay: TBStr); safecall;
    property Delay: TBStr read Get_Delay write Set_Delay;
  end;


// *********************************************************************//
// Interface: IRegistrationTrigger
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C8FEC3A-C218-4E0C-B23D-629024DB91A2}
// *********************************************************************//
  IRegistrationTrigger = interface(ITrigger)
    ['{4C8FEC3A-C218-4E0C-B23D-629024DB91A2}']
    function Get_Delay: TBStr; safecall;
    procedure Set_Delay(pDelay: TBStr); safecall;
    property Delay: TBStr read Get_Delay write Set_Delay;
  end;



// *********************************************************************//
// Interface: IExecAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}
// *********************************************************************//
  IExecAction = interface(IAction)
    ['{4C3D624D-FD6B-49A3-B9B7-09CB3CD3F047}']
    function Get_Path: TBStr; safecall;
    procedure Set_Path(pPath: TBStr); safecall;
    function Get_Arguments: TBStr; safecall;
    procedure Set_Arguments(pArgument: TBStr); safecall;
    function Get_WorkingDirectory: TBStr; safecall;
    procedure Set_WorkingDirectory(pWorkingDirectory: TBStr); safecall;
    property Path: TBStr read Get_Path write Set_Path;
    property Arguments: TBStr read Get_Arguments write Set_Arguments;
    property WorkingDirectory: TBStr read Get_WorkingDirectory write Set_WorkingDirectory;
  end;

// *********************************************************************//
// Interface: IShowMessageAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {505E9E68-AF89-46B8-A30F-56162A83D537}
// *********************************************************************//
  IShowMessageAction = interface(IAction)
    ['{505E9E68-AF89-46B8-A30F-56162A83D537}']
    function Get_Title: TBStr; safecall;
    procedure Set_Title(pTitle: TBStr); safecall;
    function Get_MessageBody: TBStr; safecall;
    procedure Set_MessageBody(pMessageBody: TBStr); safecall;
    property Title: TBStr read Get_Title write Set_Title;
    property MessageBody: TBStr read Get_MessageBody write Set_MessageBody;
  end;

// *********************************************************************//
// Interface: IComHandlerAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}
// *********************************************************************//
  IComHandlerAction = interface(IAction)
    ['{6D2FD252-75C5-4F66-90BA-2A7D8CC3039F}']
    function Get_ClassId: TBStr; safecall;
    procedure Set_ClassId(pClsid: TBStr); safecall;
    function Get_Data: TBStr; safecall;
    procedure Set_Data(pData: TBStr); safecall;
    property ClassId: TBStr read Get_ClassId write Set_ClassId;
    property Data: TBStr read Get_Data write Set_Data;
  end;

// *********************************************************************//
// Interface: IEmailAction
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {10F62C64-7E16-4314-A0C2-0C3683F99D40}
// *********************************************************************//
  IEmailAction = interface(IAction)
    ['{10F62C64-7E16-4314-A0C2-0C3683F99D40}']
    function Get_Server: TBStr; safecall;
    procedure Set_Server(pServer: TBStr); safecall;
    function Get_Subject: TBStr; safecall;
    procedure Set_Subject(pSubject: TBStr); safecall;
    function Get_To_: TBStr; safecall;
    procedure Set_To_(pTo: TBStr); safecall;
    function Get_Cc: TBStr; safecall;
    procedure Set_Cc(pCc: TBStr); safecall;
    function Get_Bcc: TBStr; safecall;
    procedure Set_Bcc(pBcc: TBStr); safecall;
    function Get_ReplyTo: TBStr; safecall;
    procedure Set_ReplyTo(pReplyTo: TBStr); safecall;
    function Get_From: TBStr; safecall;
    procedure Set_From(pFrom: TBStr); safecall;
    function Get_HeaderFields: ITaskNamedValueCollection; safecall;
    procedure Set_HeaderFields(ppHeaderFields: ITaskNamedValueCollection); safecall;
    function Get_Body: TBStr; safecall;
    procedure Set_Body(pBody: TBStr); safecall;
    function Get_Attachments: PSafeArray; safecall;
    procedure Set_Attachments(pAttachements: PSafeArray); safecall;
    property Server: TBStr read Get_Server write Set_Server;
    property Subject: TBStr read Get_Subject write Set_Subject;
    property To_: TBStr read Get_To_ write Set_To_;
    property Cc: TBStr read Get_Cc write Set_Cc;
    property Bcc: TBStr read Get_Bcc write Set_Bcc;
    property ReplyTo: TBStr read Get_ReplyTo write Set_ReplyTo;
    property From: TBStr read Get_From write Set_From;
    property HeaderFields: ITaskNamedValueCollection read Get_HeaderFields write Set_HeaderFields;
    property Body: TBStr read Get_Body write Set_Body;
    property Attachments: PSafeArray read Get_Attachments write Set_Attachments;
  end;

// *********************************************************************//
// Interface: ITaskSettings2
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}
// *********************************************************************//
  ITaskSettings2 = interface(IDispatch)
    ['{2C05C3F0-6EED-4C05-A15F-ED7D7A98A369}']
    function Get_DisallowStartOnRemoteAppSession: WordBool; safecall;
    procedure Set_DisallowStartOnRemoteAppSession(pDisallowStart: WordBool); safecall;
    function Get_UseUnifiedSchedulingEngine: WordBool; safecall;
    procedure Set_UseUnifiedSchedulingEngine(pUseUnifiedEngine: WordBool); safecall;
    property DisallowStartOnRemoteAppSession: WordBool read Get_DisallowStartOnRemoteAppSession write Set_DisallowStartOnRemoteAppSession;
    property UseUnifiedSchedulingEngine: WordBool read Get_UseUnifiedSchedulingEngine write Set_UseUnifiedSchedulingEngine;
  end;

implementation

end.
