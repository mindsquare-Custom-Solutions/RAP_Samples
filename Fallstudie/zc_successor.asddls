@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Released Objects: Object Successors'

@UI.headerInfo: {
  typeName: 'Object',
  typeNamePlural: 'Objects'
}

@UI.lineItem: [{ qualifier: 'Successor', criticality: '_Successor._ReleaseState.Criticality' }]
define view entity ZC_RO_ObjectSuccessor
  as select from ZI_RO_ObjectSuccessor
{
  key SystemVersion,
  key ObjectSuccessorUuid,

@UI.lineItem: [{ qualifier: 'Predecessor', position: 10 }]
      TadirObject,
      
      @UI.lineItem: [{ qualifier: 'Predecessor', position: 20 }]
      TadirObjectName,
      
      @UI.lineItem: [{ qualifier: 'Predecessor', position: 30 }]
      ObjectType as RealObjectType,
      
      @UI.lineItem: [
        { qualifier: 'Predecessor', position: 40 },
        { qualifier: 'Predecessor', position: 50, criticality: '_Object._ReleaseState.Criticality', value: '_Object.ReleaseState' }
      ]
      ObjectKey,

      @UI.lineItem: [{ qualifier: 'Successor', position: 10 }]
      SuccessorObject,

      @UI.lineItem: [{ qualifier: 'Successor', position: 20 }]
      SuccessorObjectName,

      @UI.lineItem: [{ qualifier: 'Successor', position: 30 }]
      SuccessorObjectType,

      @UI.lineItem: [
        { qualifier: 'Successor', position: 40 },
        { qualifier: 'Successor', position: 50, criticality: '_Successor._ReleaseState.Criticality', value: '_Successor.ReleaseState' }
      ]
      SuccessorObjectKey,

      LocalLastChangedOn,

      /* Associations */
      _Object,
      _Successor,
      _SystemVersion
}
