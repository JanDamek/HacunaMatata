
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  Contants and user messages
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$IFNDEF UNIDACPRO}

{$I Sdac.inc}

unit MSConsts;
{$ENDIF}
{$ENDIF}

interface

const
  {$EXTERNALSYM WSABASEERR}
  WSABASEERR                 = 10000;
  {$EXTERNALSYM WSAECONNRESET}
  WSAECONNRESET              = (WSABASEERR+54);

{$IFDEF CLR}
const /// ??? Compiler Error
{$ELSE}
resourcestring
{$ENDIF}
  SOLEDBError             = 'OLE DB error occured. Code %Xh';
  SOLEDBNotFound          = 'OLE DB not found';
  SMSSQLNotFound          = 'Required provider is not installed';
  SNoResultSet            = 'Query must return exactly one result set - use Execute';
  SBadStatementType       = 'Bad statement type';
  SBadOutputParam         = 'Bad parameter %s. Output parameters "text", "ntext" or "image" are not acceptable';
  SBadFieldType           = 'Unknown field type "%s" (OLE DB code = %Xh)';
  SBadSQLObjectName       = 'SQL Object name must be provided';
  SBookmarksRequired      = 'Dataset does not support bookmarks, which are required for multi-record data controls';
  SCUandServerCursors     = 'CachedUpdates not allowed for server cursors';
  SOpenNextPreparedSQL    = 'Cannot OpenNext if statement is Prepared';
  SCursorTypeChanged      = 'Cursor type changed for statement';
  SObjectOpen             = 'Cannot execute command in current transaction context ' +
                            'until previous opened DataSet (CursorType is ctDefaultResultSet) is fetched to end';
  SNoKeyFields            = 'Key fields not found';
  SConnectionOpen         = 'Cannot perform this operation on an open connection';
  SBadProviderName        = 'Wrong provider name';
  SWrongDatabaseName      = 'Changing database name to default value is not allowed';

  SBadDatabaseFile        = 'Wrong Database file';
  SBadNetworkLibrary      = 'Wrong NetworkLibrary value';
  SBadPacketSize          = 'Wrong PacketSize value';
  SBadEncrypt             = 'Wrong Encrypt value';

  // see MSDN at oledb.chm::/htm/oledbstatus.htm
  SInvalidParamType       = 'invalid ParamType';
  SBadAccessor            = 'bad accessor';
  SInvalidValue           = 'invalid Value';
  SSignMismatch           = 'sign mismatch';
  SDataOverflow           = 'data overflow';
  SDataTruncated          = 'data truncated';
  SOutOfMemory            = 'could not allocate memory for return data';
  SCantCreate             = 'the provider could not allocate memory in which to return data or a storage object was already open on the rowset. ';
  SUnavaible              = 'could not obtain the value';
  SPermissionDenied       = 'user did not have permission to write to the column';
  SIntegrityViolation     = 'the data value violates the integrity constraints for the column';
  SShemaViolation         = 'the data value violates the schema''s constraint for the column';
  SBadStatus              = 'bad status';
  SUnknownStatus          = 'unknown status';

  SParamNameMissing       = 'Parameter name missing';
  SParamValueMissing      = 'Parameter value missing';
  SInvalidChar            = 'Invalid character in non-quoted parameter value';
  SParamNameUnknown       = 'Parameter name is unknown - %s';
  SBadParamValue          = 'Bad parameter value - %s=%s';

  SCheckConnection        = '/* Check connection */';

  STimestampFieldRequired    = 'Timestamp field required';
  SInvalidServerVersion      = 'Invalid server version';
  SServiceNotDefined         = 'Service not defined';
  STargetServiceNotDefined   = 'Target service not defined';
  SServiceBrokerAsync        = 'Cannot perform this operation in AsyncNotification mode';
  SDialogActive              = 'Cannot perform this operation with open conversations';
  SConnectionClosed          = 'Can''t perform operation on closed connection';

  SLocalSortingServerCursor  = 'Local sorting is not compatible with server cursor types';

  // DBROWSTATUSENUM
  SRowMultipleChanges        = 'Updating or deleting a single row caused more than one row to be updated or deleted in the data store';
  SRowPendingChanges         = 'A row had a pending change';
  SRowCanceled               = 'Updating a row was canceled during notification';
  SRowConcurrencyViolation   = 'A row was being updated or deleted, and the value of a column in that row has been changed since it was last fetched';
  SRowDeleted                = 'A row was deleted';
  SRowIntegrityViolation     = 'A row was being inserted, updated or deleted, and doing so violated the integrity constraints for the column or table';
  SRowPermissionDenied       = 'The consumer did not have sufficient permission to update, delete, or insert a row';
  SRowLimitReached           = 'The update, delete, or insert failed due to reaching a limit on the server, such as a query execution timing out';
  SRowSchemaViolation        = 'A row was being inserted, no value was specified for a column, the column does not have a default, and the column is non-nullable';
  SRowFail                   = 'The consumer encountered a recoverable, provider-specific error, such as an RPC failure when transmitting the change to a remote server';

  // Query Notification
  SInvalidQNStatement        = 'SELECT statement does not meet the requirements for query notification';
  SStatementNotSupported     = 'Statement does not support notifications';
  SSPreviousInvalid          = 'A previous command in the transaction contained a statement that does not support notifications.';
  SInvalidQNSetOptions       = 'The connection options were not set appropriately when the command was submitted';
  SInvalidQNIsolation        = 'The isolation level is not valid for query notification';
  STemplateLimit             = 'A table specified in the query has reached the maximum number of internal templates';
  SSubscriptionTimedOut      = 'The subscription timeout expired';
  SQNObjectDropped           = 'One of the underlying objects used by the query was dropped';
  SQNObjectAltered           = 'One of the underlying objects used by the query was modified';
  SSQLServerStarted          = 'SQL Server started';
  SSQLInternalError          = 'An internal error occurred in SQL Server';
  SSubscriptionRemoved       = 'The notification subscription was removed due to the state of SQL Server';

  SInvalidNotificationTimeout= 'Specified notification timeout is out of range';
  SCompactEditionNotSupported= 'SQL Server Compact Edition does not support Query Notifications';
  SDBVerAndCompactVerDiffer  = 'The database file cannot be open with the current version of SQL Server Compact Edition';
  SDBConnectionFailed        = 'Connection to database failed';
  SDBConnectionFailedMsg     = 'Connection to database failed with message:' + #13#10 + '%s';
  //SChangeNotificationNeedMARS= 'Change Notification requires MARS enabled. You should set the TMSConnection.Options.MultipleActiveResultSets property to True.';

  SMustBeInTransaction       = 'Transaction must be active to perform lock';

  SSQLNCLINeeds              = 'Provider must be SQL Native Client for nonblocking execute';
  SSQLNCLINeedsChangePwd     = 'Provider must be SQL Native Client for changing password';

  SAsynchExecuting           = 'Operation cannot be performed while executing asynchronously';

  SNumericOverflow           = 'Numeric overflow';

  SDMandServerCursors        = 'DisconnectedMode not allowed for server cursors';

  SProviderSQLOLEDB          = 'SQLOLEDB.1';
  SProviderNativeClient      = 'SQLNCLI.1';
  SProviderNativeClient10    = 'SQLNCLI10.1';
  SProviderCompact           = 'MICROSOFT.SQLSERVER.MOBILE.OLEDB.3.0';
  SProviderCompact35         = 'MICROSOFT.SQLSERVER.MOBILE.OLEDB.3.5';
  SProviderCompact40         = 'MICROSOFT.SQLSERVER.MOBILE.OLEDB.4.0';
  SLoaderNotSupport          = 'TMSLoader does not support SQL Server Compact Edition';
  SBaseTableCursors          = 'BaseTable cursor is allowed only for TMSTable';
  SNoNextResultSet           = 'Query does not return next result set';
  SUnallowableCursorType     = 'Unallowable cursor type. Supported only ctDefaultResultSet.';

  SSocketError               = 'Socket error: %d';

  SBHAddConstraints          = '-- '#$D#$A +
                               '-- Adding tables constraints'#$D#$A +
                               '-- '#$D#$A#$D#$A;

  SBHDropConstraints         = '-- '#$D#$A +
                               '-- Droping tables constraints'#$D#$A +
                               '-- '#$D#$A#$D#$A;

  STableTypeNameNotDefined    = 'TableTypeName not defined';
  SPrepareNotSupported        = 'Prepare is not supported for data of table type';
  STableTypeNotSupported      = 'Table-Valued Parameter type is not supported by SQL Server';

  SFieldNameNotDefined        = 'Field name must be defined';
  SWrongBlob                  = 'Blob field %s is empty';
  SEmptyFSTransactionContext  = 'Cannot get FILESTREAM transaction context';
  SRecordsetBookmarksRequired = 'Recordset does not support bookmarks, which are required for fetching data';

  SLocaleIdentifierUnknown    = 'Unknown locale identifier';

  SAzureNotSupportMetaDataKind = 'SQL Azure does not support this metadata kind';

implementation

end.