@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Released Objects: Objects'
@ObjectModel.usageType:{
  serviceQuality: #A,
  sizeCategory: #XL,
  dataClass: #TRANSACTIONAL
}
@ObjectModel.semanticKey: [ 'TadirObjectName' ]

@Search.searchable: true

@UI.headerInfo: {
  title: {
    value: 'ObjectKey',
    criticality: '_ReleaseState.Criticality'
  },
  description.value: '_ObjectType.Description',
  typeName: 'Object',
  typeNamePlural: 'Objects'
}
@UI.lineItem: [{criticality: '_ReleaseState.Criticality'}]
define view entity ZC_RO_Object
  as select from ZI_RO_Object
  association [0..*] to ZC_RO_ObjectSuccessor as _Successor     on  $projection.SystemVersion   = _Successor.SystemVersion
                                                                and $projection.TadirObject     = _Successor.TadirObject
                                                                and $projection.TadirObjectName = _Successor.TadirObjectName
                                                                and $projection.RealObjectType  = _Successor.RealObjectType
                                                                and $projection.ObjectKey       = _Successor.ObjectKey
  association [0..*] to ZC_RO_ObjectSuccessor as _Predecessor   on  $projection.SystemVersion   = _Predecessor.SystemVersion
                                                                and $projection.TadirObject     = _Predecessor.SuccessorObject
                                                                and $projection.TadirObjectName = _Predecessor.SuccessorObjectName
                                                                and $projection.RealObjectType  = _Predecessor.SuccessorObjectType
                                                                and $projection.ObjectKey       = _Predecessor.SuccessorObjectKey
  association [1..*] to ZC_RO_Object          as _ObjectVersion on  $projection.TadirObject     = _ObjectVersion.TadirObject
                                                                and $projection.TadirObjectName = _ObjectVersion.TadirObjectName
                                                                and $projection.RealObjectType  = _ObjectVersion.RealObjectType
                                                                and $projection.ObjectKey       = _ObjectVersion.ObjectKey
{
      @UI.facet: [{
        id: 'ObjectCollection',
        type: #COLLECTION,
        label: 'Object',
        position: 10,
        purpose: #STANDARD
      },{
        id: 'ObjectIdentification',
        type: #FIELDGROUP_REFERENCE,
        label: 'Object Identification',
        parentId: 'ObjectCollection',
        targetQualifier: 'Identification',
        position: 10
      },{
        id: 'ObjectComponent',
        type: #FIELDGROUP_REFERENCE,
        label: 'Object Component',
        parentId: 'ObjectCollection',
        targetQualifier: 'Component',
        position: 20
      },{
        id: 'ObjectHeader',
        type: #FIELDGROUP_REFERENCE,
        purpose: #HEADER,
        targetQualifier: 'ObjectHeader'
      },{
        id: 'ObjectSuccessor',
        type: #LINEITEM_REFERENCE,
        position: 20,
        purpose: #STANDARD,
        label: 'Successor Objects',
        targetElement: '_Successor',
        targetQualifier: 'Successor'
      },{
        id: 'ObjectPredecessor',
        type: #LINEITEM_REFERENCE,
        position: 30,
        purpose: #STANDARD,
        label: 'Predecessor Objects',
        targetElement: '_Predecessor',
        targetQualifier: 'Predecessor'
      },{
        id: 'ObjectVersionHistory',
        type: #LINEITEM_REFERENCE,
        position: 40,
        purpose: #STANDARD,
        label: 'Version History',
        targetElement: '_ObjectVersion',
        targetQualifier: 'ObjectVersion'
      }]

      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RO_SystemVersionVH', element: 'SystemVersion' } }]
      @Consumption.filter: {
        selectionType: #SINGLE,
        mandatory: true
      }
      @UI.selectionField: [{ position: 10 }]
      @UI.textArrangement: #TEXT_ONLY
      @UI.fieldGroup: [{ qualifier: 'ObjectHeader', position: 10 }]
      @UI.lineItem: [{ qualifier: 'ObjectVersion', position: 10, importance: #HIGH, criticality: '_ReleaseState.Criticality' }]
  key SystemVersion,

      @UI.textArrangement: #TEXT_FIRST
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RO_ObjectTypeTadirVH', element: 'Object' } }]
      @UI.selectionField: [{ position: 20 }]
      @UI.fieldGroup: [{ qualifier: 'Identification', position: 10 }]
      @UI.lineItem: [{ qualifier: 'ObjectVersion', position: 15, importance: #MEDIUM }]
  key TadirObject,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #MEDIUM
      @UI.fieldGroup: [{ qualifier: 'Identification', position: 20 }]
      @UI.lineItem: [{ qualifier: 'ObjectVersion', position: 20, importance: #MEDIUM }]
  key TadirObjectName,

      @UI.lineItem: [
        { position: 30, importance: #HIGH },
        { qualifier: 'ObjectVersion', position: 30, importance: #HIGH }
      ]
      @UI.textArrangement: #TEXT_FIRST
      @UI.fieldGroup: [{ qualifier: 'Identification', position: 30 }]
  key ObjectType as RealObjectType,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #HIGH
      @UI.lineItem: [
        { position: 40, importance: #HIGH },
        { qualifier: 'ObjectVersion', position: 40, importance: #HIGH }
      ]
      @UI.fieldGroup: [{ qualifier: 'Identification', position: 40 }]
  key ObjectKey,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      @UI.lineItem: [{ position: 50, importance: #LOW }]
      @UI.fieldGroup: [{ qualifier: 'Component', position: 10 }]
      SoftwareComponent,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Search.ranking: #LOW
      @UI.lineItem: [{ position: 60, importance: #LOW }]
      @UI.fieldGroup: [{ qualifier: 'Component', position: 20 }]
      ApplicationComponent,

      @UI.lineItem: [{ position: 45, importance: #MEDIUM, criticality: '_ReleaseState.Criticality' }]
      @UI.textArrangement: #TEXT_ONLY
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RO_ReleaseStateVH', element: 'ReleaseState' } }]
      @UI.selectionField: [{ position: 20 }]
      @UI.fieldGroup: [{ qualifier: 'ObjectHeader', position: 20, criticality: '_ReleaseState.Criticality' }]
      ReleaseState,

      LocalLastChangedOn,

      /* Associations */
      _ObjectType,
      _ReleaseState,
      _Successor,
      _Predecessor,
      _SystemVersion,
      _TadirObject,
      _ObjectVersion
}
