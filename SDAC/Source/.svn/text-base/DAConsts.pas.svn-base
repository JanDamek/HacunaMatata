
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2012 Devart. All right reserved.
//  DB Access constants
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAConsts;
{$ENDIF}
interface

const
{$IFDEF CLR}
  LineSeparator = #$D#$A;
{$ELSE}
  LineSeparator = #$D;
{$ENDIF}

  // TStringList line separator
{$IFDEF MSWINDOWS}
  SLLineSeparator = #$D#$A;
{$ELSE}
  SLLineSeparator = #$A;
{$ENDIF}
  Tabulation = #9;
  MaxUTF8CharLen = 3;

resourcestring
  SUnknownDataType       = 'Unknown data type';
  SDataTypeNotSupported  = 'Data type is not supported';
  SFieldNotFound         = 'Field %s not found';
  SAttributeNotFount     = 'Attribute %s not found';
  SCannotConvertType     = 'Cannot convert type';
  SIllegalFilter         = 'Illegal filter expression';
  SNeedBlobType          = 'Field is not BLOB';
  SInvalidSharedObject   = 'Invalid SharedObject';
  SInvalidBlob           = 'Invalid BLOB';
  SBlobMustBeCached      = 'Blob must be in cached mode';
  SCachedAlreadyEnabled  = 'Cached is already enabled';
  SKeyFieldsRequired     = 'Key fields required to call FindKey or FindNearest';
  SKeyFieldsReq          = 'Unique keyfields required';
  SNoKeyFields           = 'Key fields not found';
  SBadTableInfoName      = 'TableInfo name must be provided';
  SBadStatementType      = 'Bad statement type';
  SBadUpdatingTable      = 'Wrong UpdatingTable value - table %s is unknown';
  SKeyFieldNotFound      = '%s: Key field ''%s'' not found';
  SNotRows               = 'SQL statement doesn''t return rows';

  SInvalidUnComprBlobSize = 'Invalid uncompressed blob size';
  SInvalidComprBlobSize   = 'Invalid compressed blob size';
  SInvalidComprBlobHeader = 'Invalid compressed blob header';
  SInvalidComprBlobData   = 'Invalid compressed blob data';

  SDataSetIsNotPrepared  = 'DataSet is not prepared';
  SInvalidKeyField       = 'Error in KeyFields string';
  SNotCachedUpdate       = 'Not in cached update mode';
  SUpdateWrongDB         = 'Cannot update, %s is not owned by %s';

  SConnectionNotDefined   = 'Connection is not defined';
  SConnectionNotConnected = 'Connection is not connected';
  SCannotConnect          = 'Cannot connect to database';
  SMacroNotFound          = 'Macro %s not found';
  SNotInTransaction       = 'Can''t perform operation on inactive transaction';
  SInTransaction          = 'Can''t perform operation on active transaction';
  STransactionNotAssigned = 'Transaction not assigned';
  SUpdateFailed           = 'Update failed. Found %d records';
  SCustomUpdateFailed     = 'Update failed';
  SRefreshFailed          = 'Refresh failed. Found %d records';
  SInvalidFetchRows       = 'FetchRows must be 1..65535';
  SNoCorrespondParam      = 'Not found field corresponding parameter %s';
  SUnknownParamDataType   = 'Unknown data type of parameter %s';
  SRecordChanged          = 'Record was changed by another user';
  STableNameNotDefined    = 'TableName must be defined';
  SStoredProcNotDefined   = 'StoredProcName must be defined';
  SConnectionIsClosed     = 'Operation is not allowed when the connection is closed';
  SCannotPerformIfPooling = 'Operation is not allowed when Pooling is True';
  SMaxConnectionsReached  = 'Maximum connections reached in pool';
  SDataSetNotDefined      = 'DataSet must be defined';

  SCannotChangeIsUnicode  = 'Cannot change IsUnicode if Size > 0';
  SColumnNotFound         = 'Column %s not found';
  SNoTimers = 'Not enough timers available';

  sBlobNotCompressed      = 'Blob is not compressed';
  SCompressorNotLinked    = 'Compressor function is not linked';
  SUncompressorNotLinked  = 'Uncompressor function is not linked';

  SUpdateComponentCircularReferences = 'Circular references are not allowed between TCustomDADataset and TCustomDAUpdateSQL objects';
  SUpdateComponentInvalidType        = 'Only %s and %s objects are allowed in update properties';
  SUpdateObjectEmptySQL              = 'SQL property of InsertObject, ModifyObject, DeleteObject or RefreshObject cannot be empty';  

  SDateEncodeError        = 'Invalid argument to date encode';
  SInvalidXML             = 'Invalid XML document';
  
  SWrongTblCount          = 'To execute BackupQuery TableNames must have not more than one table';
  SBackupQueryWrongTableCount = 'Query must contain one table';
  SBHCaption              = '-- %s version: %s'#$D#$A +
                            '-- %s server version: %s'#$D#$A +
                            '-- %s client version: %s'#$D#$A +
                            '-- Script date %s'#$D#$A +
                            '-- ---------------------------------------------------------------------- '#$D#$A +
                            '-- Server: %s'#$D#$A +
                            '-- Database: %s'#$D#$A#$D#$A;
  SBHTableData            = '-- '#$D#$A +
                            '-- Dumping data for table %s'#$D#$A +
                            '-- '#$D#$A#$D#$A;

  SAreYouSureRestore      = 'Are you sure to restore data?';

  SInvalidBatchMove       = 'Invalid batch move parameters';
  SCannotFindField        = 'Cannot find field';

  SInvalidLexem           = 'Invalid lexem "%s" at position %d in statement:'#$D#$A'%s';

  SEmptySQLStatement      = 'No SQL statement provided';

  SInvalidBlobPosition    = 'Invalid BLOB position';

  SNoConnectionsInTransaction = 'There are no connections in the transaction';
  SMultiConnectionsInTransaction = 'There are more than one connection in the transaction';
  SConnectionInTransactionNotActive = 'One of the connections in the transaction is not active';
  SUnsupportedIsolationLevel = 'Unsupported transaction isolation level';
  SIsolationLevelNotSupportedWithMTS = 'Transaction isolation level is not supported with MTS';
  SReadOnlyTransactionNotSupported = 'Readonly transactions are not supported by the database';
  SOperationNotSupported  = 'Operation is not supported by the database';
  SMultipleTransactionsNotSupported = 'Multiple transactions are not supported by the database';
  SMTSNotSupported = 'MTS transaction coordinator is not supported by the database';

  SVarSubtypeNotSupported = 'Variant subtype is not supported: %d'; 
  SInvalidParamsArray     = 'Invalid Params array. Valid format: <Name1>,<Value1>[,<Name2>,<Value2>[...]]';
  SUnsupportedMetaDataKind = 'Unsupported metadata kind';
  SRestrictionMustBeSet   = '%s restriction must be set';

  SEventsNotDefined = 'Events names are not defined';
  SMDIsNotMemDataSet = 'Master dataset must be inherited from TMemDataSet';

{$IFDEF FPC}
  SFPCNotSupported = 'This functionality is not supported for Free Pascal for the time being';

  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  SFieldRequired = 'Field ''%s'' must have a value';
  SCircularDataLink = 'Circular datalinks are not allowed';
  sTargetBufferTooSmall = 'ZLib error: target buffer may be too small';
  sError = 'Error';
  SBcdOverflow = 'BCD overflow';
  SNotEditing = 'Dataset not in edit or insert mode';
{$ENDIF}

implementation

end.
