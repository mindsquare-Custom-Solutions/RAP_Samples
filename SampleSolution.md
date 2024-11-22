<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

# Sample solutions of the mindsquare RAP Training Unit

Sample solutions of the [mindsquare Restful Application Programming Model Training Unit](https://mindsquare.de/schulungen/)

**!!! IMPORTANT !!!**

There are preconditions listed for every step before you are able to execute the code in case you want to test it for yourself.
The preconditions don't list everything absolutely necessary for the code to be functional, mostly just the last step (else the preconditions would grow to unreadable levels with every step). Example:

To define ZC_TravelTP (the Projection View), the precondition lists ZR_TravelTP (Root View Entity) for the code to work. But for ZR_TravelTP to work, you need to define ZI_Travel (Basic View) first (and everything else listed for every step).

So, if necessary, follow the chain to the beginning and start copying from there if you haven't done so yet. Use the search function (CTRL + F) for easier navigation.

## Defining Data Models

Preconditions for this step:
- None
- (Have the necessary database tables defined)

### Basic Interface Views

#### Defining ZI_Travel

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for Travel'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.representativeKey: 'TravelId'
define view entity ZI_MS_Travel
  as select from zmind2_travel
  association [0..1] to I_Currency       as _Currency on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to zmind2_trvl_stat as _Status   on $projection.Status = _Status.travel_status
  association [0..1] to zmind2_customer  as _Customer on $projection.CustomerId = _Customer.customer_id
  association [0..1] to zmind2_agency    as _Agency   on $projection.AgencyId = _Agency.agency_id
{
  key travel_id          as TravelId,
      agency_id          as AgencyId,
      customer_id        as CustomerId,
      begin_date         as BeginDate,
      end_date           as EndDate,
      booking_fee        as BookingFee,
      total_price        as TotalPrice,
      currency_code      as CurrencyCode,
      description        as Description,
      status             as Status,
      createdby          as CreatedBy,
      createdat          as CreatedAt,
      lastchangedby      as LastChangedBy,
      lastchangedat      as LastChangedAt,
      locallastchangedat as LocalLastChangedAt,

      /* Associations */
      _Currency,
      _Status,
      _Customer,
      _Agency
}
```

#### Defining ZI_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for Booking'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.representativeKey: 'BookingId'
define view entity ZI_MS_Booking
  as select from zmind2_booking
  association [0..1] to ZI_MS_Travel     as _Travel   on $projection.TravelId = _Travel.TravelId
  association [0..1] to I_Currency       as _Currency on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to zmind2_book_stat as _Status   on $projection.BookingStatus = _Status.booking_status
  association [0..1] to zmind2_customer  as _Customer on $projection.CustomerId = _Customer.customer_id
  association [0..1] to zmind2_carrier   as _Carrier  on $projection.CarrierId = _Carrier.carrier_id
{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
      booking_date          as BookingDate,
      booking_status        as BookingStatus,
      customer_id           as CustomerId,
      carrier_id            as CarrierId,
      connection_id         as ConnectionId,
      flight_date           as FlightDate,
      flight_price          as FlightPrice,
      currency_code         as CurrencyCode,
      local_last_changed_at as LocalLastChangedAt,
      last_changed_at       as LastChangedAt,

      /* Associations */
      _Travel,
      _Currency,
      _Status,
      _Customer,
      _Carrier
}
```

## Defining Business Objects

Preconditions for this step:
- Have ZI_Travel and ZI_Booking defined

### Root View Entites

#### Defining ZR_TravelTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root View for Entity Travel'
define root view entity ZR_MS_TravelTP
  as select from ZI_MS_Travel
  composition [0..*] of ZR_MS_BookingTP as _Booking
{
  key TravelId,
      AgencyId,
      CustomerId,
      BeginDate,
      EndDate,
      BookingFee,
      TotalPrice,
      CurrencyCode,
      Description,
      Status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      _Booking, // Make association public
      _Agency,
      _Currency,
      _Customer,
      _Status
}
```

### Child View Entities

#### Defining ZR_BookingTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Child View for TravelTP'
define view entity ZR_MS_BookingTP
  as select from ZI_MS_Booking
  association to parent ZR_MS_TravelTP as _Travel on $projection.TravelId = _Travel.TravelId
{
  key TravelId,
  key BookingId,
      BookingDate,
      BookingStatus,
      CustomerId,
      CarrierId,
      ConnectionId,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      LocalLastChangedAt,
      LastChangedAt,

      /* Associations */
      _Travel, // Make association public
      _Carrier,
      _Currency,
      _Customer,
      _Status
}
```
## Defining Behaviour Definitions

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)

### Defining ZR_TravelTP and ZR_BookingTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );

define behavior for ZR_MS_TravelTP alias Travel
persistent table zmind2_travel
lock master
authorization master ( instance )
{
  create;
  update;
  delete;

  field ( readonly : update ) TravelId;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; }
}

define behavior for ZR_MS_BookingTP alias Booking
persistent table zmind2_booking
lock dependent by _Travel
authorization dependent by _Travel
{
  update;
  delete;

  field ( readonly : update ) TravelId, BookingId;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { }
}
```

### Behavior Pools

#### Defining ZBP_R_TravelTP (possible with Quick Fix)

```abap
CLASS zbp_r_ms_traveltp DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zr_ms_traveltp.
ENDCLASS.

CLASS zbp_r_ms_traveltp IMPLEMENTATION.
ENDCLASS.
```

## Defining Projections

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have ZR_TravelTP defined (Behavior Definition)

### Projection Views

#### Defining ZC_TravelTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View for Travel'
@Metadata.allowExtensions: true
define root view entity ZC_MS_TravelTP
  provider contract transactional_query
  as projection on ZR_MS_TravelTP
{
  key TravelId,
      AgencyId,
      CustomerId,
      BeginDate,
      EndDate,
      BookingFee,
      TotalPrice,
      CurrencyCode,
      Description,
      Status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      /* Associations */
      _Agency,
      _Booking : redirected to composition child ZC_MS_BookingTP,
      _Currency,
      _Customer,
      _Status
}
```

### Defining ZC_BookingTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View for Booking'
@Metadata.allowExtensions: true
define view entity ZC_MS_BookingTP
  as projection on ZR_MS_BookingTP
{
  key TravelId,
  key BookingId,
      BookingDate,
      BookingStatus,
      CustomerId,
      CarrierId,
      ConnectionId,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      LocalLastChangedAt,
      LastChangedAt,

      /* Associations */
      _Carrier,
      _Currency,
      _Customer,
      _Status,
      _Travel : redirected to parent ZC_MS_TravelTP
}
```

### Projection Behaviors

#### Defining ZC_TravelTP

```cds
projection;
strict ( 2 );

define behavior for ZC_MS_TravelTP alias Travel
{
  use create;
  use update;
  use delete;

  use association _Booking { create; }
}

define behavior for ZC_MS_BookingTP alias Booking
{
  use update;
  use delete;

  use association _Travel;
}
```

## Publishing Services

Preconditions for this step:
- Have ZC_TravelTP and ZC_BookingTP defined (Projection Views)

### Defining Service Definitions

```cds
@EndUserText.label: 'Service Defintion for Travel'
define service ZI_UI_MS_Travel {
  expose ZC_MS_TravelTP;
  expose ZC_MS_BookingTP;
  expose I_CurrencyStdVH;
  expose ZMIND2E_I_FlightVH;
  expose ZMIND2E_I_Agency;
  expose ZMIND2E_I_Customer;
  expose I_Country;
  expose ZMIND2E_I_Carrier;
}
```

### Defining Service Bindings
- Follow the instructions of the training unit

## Defining Metadata Extensions

Preconditions for this step:
- Have ZC_TravelTP and ZC_BookingTP defined (Projection Views)
- To test in Fiori Elements preview: have ZC_TravelTP defined (Projection Behavior Definition)

### Defining ZC_I_TravelTP_UI
```cds
@Metadata.layer: #PARTNER

@UI.headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title: {
        value: 'Description'
    },
    description.value: 'CustomerID'
}

@UI.presentationVariant: [{
    sortOrder: [{ by: 'BeginDate', direction: #ASC }],
    qualifier: 'pTravel',
    text: 'Default',
    visualizations: [{ type: #AS_LINEITEM, qualifier: 'Default' }]
 //   requestAtLeast: [ 'BookCriticality', 'CancelCriticality' ]
}]

@UI.selectionVariant: [{
    qualifier: 'sTravel',
    text: 'feature travels'
}]

@UI.selectionPresentationVariant: [{
    presentationVariantQualifier: 'pTravel',
    selectionVariantQualifier: 'sTravel',
    text: 'DefaultVariant'
}]

annotate entity ZC_MS_TravelTP with
{
  @UI.facet: [{
      type: #COLLECTION,
      id: 'TravelCollection',
      label: 'Travel',
      purpose: #STANDARD,
      position: 10
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'DetailsFieldGroup',
      targetQualifier: 'Details',
      parentId: 'TravelCollection',
      label: 'Details',
      position: 10
  },{
      type: #COLLECTION,
      id: 'CustomerCollection',
      label: 'Customer',
      purpose: #STANDARD,
      position: 20
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerNameFieldGroup',
      parentId: 'CustomerCollection',
      targetQualifier: 'Customer',
      label: 'Name',
      position: 10
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerContactFieldGroup',
      parentId: 'CustomerCollection',
      targetQualifier: 'CustomerContact',
      label: 'Contact',
      position: 20
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerAddressFieldGroup',
      parentId: 'CustomerCollection',
      targetQualifier: 'CustomerAddress',
      label: 'Address',
      position: 30
  },{
      type: #LINEITEM_REFERENCE,
      id: 'BookingLineItem',
      purpose: #STANDARD,
      targetElement: '_Booking',
      targetQualifier: 'Booking',
      position: 30,
      label: 'Bookings'
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'TraceFieldGroup',
      targetQualifier: 'Trace',
      purpose: #STANDARD,
      label: 'Trace',
      position: 40
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'DatesFieldGroup',
      targetQualifier: 'Dates',
      parentId: 'TravelCollection',
      label: 'Date',
      position: 20
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'PricesFieldGroup',
      targetQualifier: 'Prices',
      parentId: 'TravelCollection',
      label: 'Price',
      position: 40
  }]

  @UI.fieldGroup: [{ qualifier: 'Details', position: 10 }]
  @UI.lineItem: [{ position: 5, qualifier: 'Default' }]
  TravelId;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 20 }]
  @Consumption.valueHelpDefinition: [{
      entity: {
          name: 'ZMIND2E_I_AgencyVH',
          element: 'AgencyId'
      },
      useForValidation: true
  }]
  AgencyId;

  //  @UI.fieldGroup: [
  //    { qualifier: 'Customer', position: 10 },
  //    { qualifier: 'Customer', position: 20, value: '_Customer.Title' },
  //    { qualifier: 'Customer', position: 30, value: '_Customer.FirstName' },
  //    { qualifier: 'Customer', position: 40, value: '_Customer.LastName' },
  //    { qualifier: 'CustomerContact', position: 10, value: '_Customer.EmailAddress' },
  //    { qualifier: 'CustomerContact', position: 20, value: '_Customer.PhoneNumber' },
  //    { qualifier: 'CustomerAddress', position: 10, value: '_Customer.Street' },
  //    { qualifier: 'CustomerAddress', position: 20, value: '_Customer.City' },
  //    { qualifier: 'CustomerAddress', position: 30, value: '_Customer.PostalCode' },
  //    { qualifier: 'CustomerAddress', position: 10, value: '_Customer.CountryCode' }
  //  ]
  @UI.lineItem: [{ position: 40, qualifier: 'Default' }]
  @Consumption.valueHelpDefinition: [{
      entity: {
          name: 'ZMIND2E_I_CustomerVH',
          element: 'CustomerId'
      },
      useForValidation: false
  }]
  //  @UI.textArrangement: #TEXT_SEPARATE
  CustomerId;

  @UI.fieldGroup: [{ qualifier: 'Dates', position: 10 }]
  @UI.lineItem: [{ position: 20, qualifier: 'Default' }]
  BeginDate;

  @UI.fieldGroup: [{ qualifier: 'Dates', position: 20 }]
  @UI.lineItem: [{ position: 30, qualifier: 'Default' }]
  EndDate;

  @UI.fieldGroup: [{ qualifier: 'Prices', position: 10 }]
  @Consumption.valueHelpDefinition: [{
      entity: {
          name: 'I_CurrencyStdVH',
          element: 'Currency'
      },
      useForValidation: true
  }]
  CurrencyCode;

  @UI.fieldGroup: [{ qualifier: 'Prices', position: 30 }]
  BookingFee;

  @UI.fieldGroup: [{ qualifier: 'Prices', position: 20 }]
  @UI.lineItem: [{ position: 50, qualifier: 'Default' }]
  TotalPrice;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 30 }]
  @UI.lineItem: [{ position: 10, emphasized: true, qualifier: 'Default' }]
  Description;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 40 }]
  @UI.lineItem: [{ position: 60, qualifier: 'Default' }]
  Status;

  @UI.fieldGroup: [{ qualifier: 'Trace', position: 10 }]
  @UI.selectionField: [{ position: 50 }]
  CreatedBy;

  @UI.fieldGroup: [{ qualifier: 'Trace', position: 20 }]
  CreatedAt;

  @UI.fieldGroup: [{ qualifier: 'Trace', position: 30 }]
  LastChangedBy;

  @UI.fieldGroup: [{ qualifier: 'Trace', position: 40 }]
  LastChangedAt;

  @UI.fieldGroup: [{ qualifier: 'Trace', position: 50 }]
  LocalLastChangedAt;
}
```

### Defining ZC_I_BookingTP_UI

```cds
@Metadata.layer: #PARTNER

@UI.headerInfo: {
    typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title.value: 'BookingDate',
    description.value: 'CustomerId'
}

@UI.presentationVariant: [{
    sortOrder: [{ by: 'FlightDate', direction: #ASC }]
}]
annotate entity ZC_MS_BookingTP with
{
  @UI.facet: [{
    type: #COLLECTION,
    id: 'DetailsCollection',
    purpose: #STANDARD,
    label: 'Details',
    position: 10
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'BookingFieldGroup',
    targetQualifier: 'Booking',
    parentId: 'DetailsCollection',
    label: 'Booking',
    position: 10
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'FlightFieldGroup',
    targetQualifier: 'Flight',
    parentId: 'DetailsCollection',
    label: 'Flight',
    position: 20
  }]

  @UI.lineItem: [{ qualifier: 'Booking', position: 10 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 10 }]
  BookingId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 15 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 15 }]
  BookingStatus;

  @UI.lineItem: [{ qualifier: 'Booking', position: 20 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 20 }]
  BookingDate;

  @UI.lineItem: [{ qualifier: 'Booking', position: 30 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 30 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'CarrierId' },
    additionalBinding: [
      { element: 'ConnectionId', localElement: 'ConnectionID', usage: #RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  CarrierId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 40 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 40 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'ConnectionId' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  ConnectionId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 50 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 50 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'FlightDate' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'ConnectionId', localElement: 'ConnectionID', usage: #FILTER_AND_RESULT }
    ]
  }]
  FlightDate;

  @UI.lineItem: [{ qualifier: 'Booking', position: 60 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 60 }]
  FlightPrice;

  @Consumption.valueHelpDefinition: [{ entity.name: 'I_CurrencyStdVH', entity.element: 'Currency' }]
  CurrencyCode;

  @UI.fieldGroup: [{ qualifier: 'Booking', position: 70 }]
  LocalLastChangedAt;
}
```

## Working with Business Objects in ABAP

Preconditions for this step:
- Have ZC_TravelTP and ZC_BookingTP defined (Projection Views)
- Have ZC_TravelTP defined (Projection Behavior Definition)

### EML

#### Defining ZI_EML_Travel
- TODO

## Feature Control

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)

### Semantic Annotations

Changes:
- Added several semantic annotations to some fields

#### Adjusting ZI_Travel

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for Travel'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.representativeKey: 'TravelId'
define view entity ZI_MS_Travel
  as select from zmind2_travel
  association [0..1] to I_Currency       as _Currency on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to zmind2_trvl_stat as _Status   on $projection.Status = _Status.travel_status
  association [0..1] to zmind2_customer  as _Customer on $projection.CustomerId = _Customer.customer_id
  association [0..1] to zmind2_agency    as _Agency   on $projection.AgencyId = _Agency.agency_id
{
  key travel_id          as TravelId,
      agency_id          as AgencyId,
      customer_id        as CustomerId,
      begin_date         as BeginDate,
      end_date           as EndDate,
      booking_fee        as BookingFee,
      total_price        as TotalPrice,
      currency_code      as CurrencyCode,
      description        as Description,
      status             as Status,
      @Semantics.user.createdBy: true
      createdby          as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      createdat          as CreatedAt,
      @Semantics.user.lastChangedBy: true
      lastchangedby      as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat      as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchangedat as LocalLastChangedAt,

      /* Associations */
      _Currency,
      _Status,
      _Customer,
      _Agency
}
```

#### Adjusting ZI_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for Booking'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.representativeKey: 'BookingId'
define view entity ZI_MS_Booking
  as select from zmind2_booking
  association [0..1] to ZI_MS_Travel     as _Travel   on $projection.TravelId = _Travel.TravelId
  association [0..1] to I_Currency       as _Currency on $projection.CurrencyCode = _Currency.Currency
  association [0..1] to zmind2_book_stat as _Status   on $projection.BookingStatus = _Status.booking_status
  association [0..1] to zmind2_customer  as _Customer on $projection.CustomerId = _Customer.customer_id
  association [0..1] to zmind2_carrier   as _Carrier  on $projection.CarrierId = _Carrier.carrier_id
{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
      booking_date          as BookingDate,
      booking_status        as BookingStatus,
      customer_id           as CustomerId,
      carrier_id            as CarrierId,
      connection_id         as ConnectionId,
      flight_date           as FlightDate,
      flight_price          as FlightPrice,
      currency_code         as CurrencyCode,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,

      /* Associations */
      _Travel,
      _Currency,
      _Status,
      _Customer,
      _Carrier
}
```

### Static Feature Controls

Changes:
- Added static feature controls to all fields ( ex: "field ( readonly ) Status, ...;" )

#### Adjusting ZR_TravelTP (Behavior Definition)

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );

define behavior for ZR_MS_TravelTP alias Travel
persistent table zmind2_travel
lock master
authorization master ( instance )
{
  create;
  update;
  delete;

  field ( mandatory : create, readonly : update ) TravelId;
  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; }
}

define behavior for ZR_MS_BookingTP alias Booking
persistent table zmind2_booking
lock dependent by _Travel
authorization dependent by _Travel
{
  update;
  delete;

  field ( readonly ) TravelId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) BookingId, CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { }
}
```

## Locking

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)

### Pessimistic and Opimistic Locking

Changes:
- Added etag master LocalLastChangedAt to both ZR_TravelTP and ZR_BookingTP
- Added use etag to ZC_TravelTP for Travel and Booking
- ( Would have added lock master and lock dependent by for pessimistic locking but was already required to add by strict ( 2 ) )

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );

define behavior for ZR_MS_TravelTP alias Travel
persistent table zmind2_travel
lock master
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;

  field ( mandatory : create, readonly : update ) TravelId;
  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; }
}

define behavior for ZR_MS_BookingTP alias Booking
persistent table zmind2_booking
lock dependent by _Travel
authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) BookingId, CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { }
}
```

#### Adjusting ZC_TravelTP

```cds
projection;
strict ( 2 );

define behavior for ZC_MS_TravelTP alias Travel
use etag
{
  use create;
  use update;
  use delete;

  use association _Booking { create; }
}

define behavior for ZC_MS_BookingTP alias Booking
use etag
{
  use update;
  use delete;

  use association _Travel;
}
```

## Draft Handling

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)

### Adjusting the Behavior Definitions

Changes:
- ZR_TravelTP:
  - Added with draft;
  - Added draft tables z_d_travel and z_d_booking (can be generated with Quick Fix)
  - Added total etag
  - Added several draft actions
  - Added with draft; in associations
- ZC_TravelTP:
  - Added use draft;
  - Added use etag
  - Added draft actions used in ZR_TravelTP
  - Added with draft; in associations

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {

  }

  field ( mandatory : create, readonly : update ) TravelId;
  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) BookingId, CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Adjusting ZC_TravelTP

```cds
projection;
strict ( 2 );
use draft;

define behavior for ZC_MS_TravelTP alias Travel
use etag
{
  use create;
  use update;
  use delete;

  use action Activate;
  use action Discard;
  use action Edit;
  use action Prepare;
  use action Resume;

  use association _Booking { create; with draft; }
}

define behavior for ZC_MS_BookingTP alias Booking
{
  use update;
  use delete;

  use association _Travel { with draft; }
}
```

### Generating Draft Tables (via Quick Fix)

Note:
- Nothing was changed after generating via Quick Fix

#### Generating z_d_travel

```cds
@EndUserText.label : 'Draft Table for Entity ZR_MS_TravelTP'
@AbapCatalog.enhancement.category : #EXTENSIBLE_ANY
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zms_d_travel {

  key mandt          : mandt not null;
  key travelid       : zmind2_travel_id not null;
  agencyid           : zmind2_agency_id;
  customerid         : zmind2_customer_id;
  begindate          : zmind2_begin_date;
  enddate            : zmind2_end_date;
  @Semantics.amount.currencyCode : 'zms_d_travel.currencycode'
  bookingfee         : zmind2_booking_fee;
  @Semantics.amount.currencyCode : 'zms_d_travel.currencycode'
  totalprice         : zmind2_total_price;
  currencycode       : zmind2_currency_code;
  description        : zmind2_description;
  status             : zmind2_travel_status;
  createdby          : abp_creation_user;
  createdat          : abp_creation_tstmpl;
  lastchangedby      : abp_lastchange_user;
  lastchangedat      : abp_lastchange_tstmpl;
  locallastchangedat : abp_locinst_lastchange_tstmpl;
  "%admin"           : include sych_bdl_draft_admin_inc;
}
```

#### Generating z_d_booking

```cds
@EndUserText.label : 'Draft Table for Entity ZR_MS_BookingTP'
@AbapCatalog.enhancement.category : #EXTENSIBLE_ANY
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zms_d_booking {

  key mandt          : mandt not null;
  key travelid       : zmind2_travel_id not null;
  key bookingid      : zmind2_booking_id not null;
  bookingdate        : zmind2_booking_date;
  bookingstatus      : zmind2_booking_status;
  customerid         : zmind2_customer_id;
  carrierid          : zmind2_carrier_id;
  connectionid       : zmind2_connection_id;
  flightdate         : zmind2_flight_date;
  @Semantics.amount.currencyCode : 'zms_d_booking.currencycode'
  flightprice        : zmind2_flight_price;
  currencycode       : zmind2_currency_code;
  locallastchangedat : abp_locinst_lastchange_tstmpl;
  lastchangedat      : abp_lastchange_tstmpl;
  "%admin"           : include sych_bdl_draft_admin_inc;

}
```

## Numbering

Preconditions for this step:
- HAVE to have defined the Number Range used for the object in the early numbering method (in the SAPGUI), otherwise code won't function at all (TODO: Transaktion einfügen, in der man das macht, ggf. Tutorial in den Schulungsfolien?)
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)

### Early Numbering

Changes:
- ZR_TravelTP
  - Added early numbering for Travel and Booking
  - Changed TravelId in Travel to readonly (due to early numbering)
  - Changed BookingId in Booking to readonly (due to early numbering)
- ZBP_R_TravelTP
  - Added earlynumbering_create and earlynumbering_cba_booking (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))
 
#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {

  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating earlynumbering_create in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.
```

#### Creating earlynumbering_create in lhc_Travel (implementation)

```abap
METHOD earlynumbering_create.
    DATA:
      entity        TYPE STRUCTURE FOR CREATE ZR_MS_TravelTP,
      travel_id_max TYPE zmind2_travel_id.

    LOOP AT entities INTO entity WHERE TravelId IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-travel.
    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    DELETE entities_wo_travelid WHERE TravelId IS NOT INITIAL.

    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = 'ZI_TRVL'
            quantity          = CONV #( lines( entities_wo_travelid ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(number_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity)
        ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #(  %cid = entity-%cid
                           %key = entity-%key
                           %is_draft = entity-%is_draft
                           %msg = lx_number_ranges
                        ) TO reported-travel.
          APPEND VALUE #(  %cid = entity-%cid
                           %key = entity-%key
                           %is_draft = entity-%is_draft
                        ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDTRY.

    ASSERT number_range_returned_quantity = lines( entities_wo_travelid ).

    travel_id_max = number_range_key - number_range_returned_quantity.

    LOOP AT entities_wo_travelid INTO entity.
      travel_id_max += 1.
      entity-TravelId = travel_id_max.

      APPEND VALUE #( %cid  = entity-%cid
                      %key  = entity-%key
                      %is_draft = entity-%is_draft
                      TravelId = entity-TravelId
                    ) TO mapped-travel.
    ENDLOOP.

  ENDMETHOD.
```

#### Creating earlynumbering_cba_booking in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\_booking.
```

#### Creating earlynumbering_cba_booking in lhc_Travel (implementation)

```abap
METHOD earlynumbering_cba_booking.
    DATA: max_booking_id TYPE zmind2_booking_id.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY travel BY \_booking
        FROM CORRESPONDING #( entities )
        LINK DATA(bookings).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel_group>) GROUP BY <travel_group>-travelid.

      max_booking_id = REDUCE #( INIT max = CONV ZMIND2_booking_id( '0' )
                                 FOR  booking IN bookings USING KEY entity WHERE ( source-travelid  = <travel_group>-travelid )
                                 NEXT max = COND ZMIND2_booking_id( WHEN booking-target-bookingid > max
                                                                    THEN booking-target-bookingid
                                                                    ELSE max )
                               ).
      max_booking_id = REDUCE #( INIT max = max_booking_id
                                 FOR  entity IN entities USING KEY entity WHERE ( travelid  = <travel_group>-travelid )
                                 FOR  target IN entity-%target
                                 NEXT max = COND ZMIND2_booking_id( WHEN   target-bookingid > max
                                                                    THEN target-bookingid
                                                                    ELSE max )
                               ).

      LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel>) USING KEY entity WHERE travelid = <travel_group>-travelid.

        LOOP AT <travel>-%target ASSIGNING FIELD-SYMBOL(<booking_wo_numbers>).
          APPEND CORRESPONDING #( <booking_wo_numbers> ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
          IF <booking_wo_numbers>-bookingid IS INITIAL.
            max_booking_id += 1 .
            <mapped_booking>-bookingid = max_booking_id .
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.
```

## Determinations

### Creating a Message Class (for all future uses)
| Number | Short Text |
|--------|------------|
| 001    | The start date &1 must be before the end date &2.|
| 002    | Please enter a customer. |
| 003    | Please enter something. |
| 004    | You cannot currently book a trip. |
| 005    | Trip &1 has already been canceled. |
| 006    | Trip &1 cannot be deleted. |
| 007    | Invalid entry for the currency. |
| 008    | Unfortunately there is no conversion for &1. |
| 009    | You have no authorization for AgencyId &1. |
| 010    | Successfully converted the currency. |


Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have implemented everything necessary for early numbering

### Setting Standard Values

Changes:
- ZR_TravelTP
  - Added determinations for Travel and Booking
- ZBP_R_TravelTP
  - Added setStatusOnCreate, setBookingStatus and setBookingDate (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {

  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating setStatusOnCreate in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS setstatusoncreate FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~setstatusoncreate.
```

#### Creating setStatusOnCreate in lhc_Travel (implementation)

```abap
METHOD setStatusOnCreate.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
    ENTITY Travel
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    DELETE travels WHERE Status IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Travel
            UPDATE FIELDS ( Status )
            WITH VALUE #( FOR travel IN travels
                          ( %tky = travel-%tky
                            Status = 'P' ) ).
  ENDMETHOD.
```

#### Creating setBookingStatus in lhc_Booking (definition)

```abap
PRIVATE SECTION.

    METHODS setBookingStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~setBookingStatus.
```

#### Creating setBookingStatus in lhc_Booking (implementation)

```abap
METHOD setBookingStatus.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Travel
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).


    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Travel BY \_Booking
        FIELDS ( BookingStatus )
        WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

    DATA updates TYPE TABLE FOR UPDATE ZR_MS_TravelTP\\Booking.

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>) WHERE TravelId = <travel>-TravelId.
        APPEND VALUE #( %tky = <booking>-%tky
                        BookingStatus = <travel>-Status ) TO updates.
      ENDLOOP.
    ENDLOOP.

    MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Booking
        UPDATE FIELDS ( BookingStatus )
        WITH updates.

  ENDMETHOD.
```

#### Creating setBookingDate in lhc_Booking (definition)

```abap
PRIVATE SECTION.

    METHODS setBookingDate FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Booking~setBookingDate.
```

#### Creating setBookingDate in lhc_Booking (implementation)

```abap
METHOD setBookingDate.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
    ENTITY Travel BY \_Booking
        FIELDS ( BookingDate )
        WITH CORRESPONDING #( keys )
    RESULT DATA(bookings).

    DELETE bookings WHERE BookingDate IS NOT INITIAL.
    CHECK bookings IS NOT INITIAL.


    MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Booking
            UPDATE FIELDS ( BookingDate )
            WITH VALUE #( FOR booking IN bookings
                          ( %tky = booking-%tky
                            BookingDate = cl_abap_context_info=>get_system_date( )  ) ).
  ENDMETHOD.
```

## Validations

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have implemented everything necessary for early numbering
- Have created a message class (see Determinations)

### Validation of Input Data

Changes:
- ZR_TravelTP
  - Added validations for Travel and Booking
  - Added validations in draft determine action Prepare
- ZBP_R_Travel
  - Added validateDates, validateCustomer and validateFlight (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))
 
#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete;

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {
    validation validateDates;
    validation validateCustomer;
  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  validation validateDates on save { create; update; }
  validation validateCustomer on save { field CustomerId; }

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  validation validateFlight on save { create; update; field CarrierId, ConnectionId, FlightDate; }

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating validateDates in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS validatedates FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatedates.
```

#### Creating validateDates in lhc_Travel (implementation)

```abap
METHOD validateDates.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Travel
        FIELDS ( BeginDate EndDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      IF travel-EndDate IS NOT INITIAL AND travel-BeginDate IS NOT INITIAL AND travel-EndDate < travel-BeginDate.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky     = travel-%tky
                        %msg     = new_message( id       = 'ZCM_MS_Travel'
                                                number   = '001'
                                                v1       = travel-BeginDate
                                                v2       = travel-EndDate
                                                severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on )
               TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
```

#### Creating validateCustomer in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.
```

#### Creating validateCustomer in lhc_Travel (implementation)

```abap
METHOD validateCustomer.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY travel
            FIELDS ( CustomerId )
            WITH CORRESPONDING #(  keys )
        RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF zmind2e_I_customer WITH UNIQUE KEY CustomerId.

    customers = CORRESPONDING #(  travels DISCARDING DUPLICATES MAPPING CustomerId = CustomerId EXCEPT * ).
    DELETE customers WHERE CustomerId IS INITIAL.
    IF  customers IS NOT INITIAL.

      SELECT FROM zmind2e_i_customer FIELDS CustomerId
        FOR ALL ENTRIES IN @customers
        WHERE CustomerId = @customers-CustomerId
        INTO TABLE @DATA(customers_db).
    ENDIF.

    LOOP AT travels INTO DATA(travel).
      IF travel-CustomerId IS INITIAL..

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = travel-%tky
                        %element-CustomerId = if_abap_behv=>mk-on )
               TO reported-travel.

      ELSEIF NOT line_exists( customers_db[ CustomerId = travel-CustomerId ] ).

        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = travel-%tky
                       %element-CustomerId = if_abap_behv=>mk-on )
              TO reported-travel.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.
```

#### Creating validateFlight in lhc_Booking (definition)

```abap
PRIVATE SECTION.

    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.
```

#### Creating validateFlight in lhc_Booking (implementation)

```abap
METHOD validateFlight.
  READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Booking
          FIELDS ( CarrierId ConnectionId FlightDate )
          WITH CORRESPONDING #( keys )
      RESULT DATA(bookings).

  DATA connections TYPE SORTED TABLE OF ZMIND2E_I_Flight WITH UNIQUE KEY CarrierId ConnectionId FlightDate.

  connections = CORRESPONDING #( bookings
                             DISCARDING DUPLICATES
                             MAPPING CarrierId = CarrierId
                                    ConnectionId = ConnectionId
                                    FlightDate = FlightDate
                             EXCEPT * ).

  DELETE connections WHERE CarrierId IS INITIAL
                    OR ConnectionId IS INITIAL
                    OR FlightDate IS INITIAL.

  IF connections IS NOT INITIAL.
    SELECT FROM ZMIND2E_I_Flight FIELDS CarrierId, ConnectionId, FlightDate
      FOR ALL ENTRIES IN @connections
      WHERE CarrierId = @connections-CarrierId
      AND ConnectionId = @connections-ConnectionId
      AND FlightDate = @connections-FlightDate
      INTO TABLE @DATA(connections_db).
  ENDIF.

  LOOP AT bookings INTO DATA(booking).
    IF booking-CarrierId IS INITIAL OR booking-ConnectionId IS INITIAL OR booking-FlightDate IS INITIAL.
      APPEND VALUE #( %key = booking-%key ) TO failed-booking.
      APPEND VALUE #( %key     = booking-%key
                      %msg     = new_message( id       = 'ZCM_MS_Travel'
                                              number   = '003'
                                              severity = if_abap_behv_message=>severity-error )
                      %element-CarrierId = if_abap_behv=>mk-on
                      %element-ConnectionId = if_abap_behv=>mk-on
                      %element-FlightDate = if_abap_behv=>mk-on )
             TO reported-booking.
    ELSE.
      READ TABLE connections_db TRANSPORTING NO FIELDS WITH KEY
           CarrierId = booking-CarrierId
           ConnectionId = booking-ConnectionId
           FlightDate = booking-FlightDate.

      IF sy-subrc <> 0.
        APPEND VALUE #( %key = booking-%key ) TO failed-booking.
        APPEND VALUE #( %key     = booking-%key
                        %element-CarrierId = if_abap_behv=>mk-on
                        %element-ConnectionId = if_abap_behv=>mk-on
                        %element-FlightDate = if_abap_behv=>mk-on )
               TO reported-booking.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
```

## Actions

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have implemented everything necessary for early numbering
- Have created a message class (see Determinations)

### Actions with Feature Controls

Changes:
- ZR_TravelTP
  - Added actions for Travel
  - Added features: instance for delete in Travel
- ZBP_R_Travel
  - Added bookTravel, cancelTravel and get_instance_features (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))
- ZC_TravelTP (Projection View)
  - Added the actions to the Fiori Elements App
- ZC_TravelTP (Projection Behavior Definition)
  - Added the actions used in ZR_TravelTP

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete ( features : instance );

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {
    validation validateDates;
    validation validateCustomer;
  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  validation validateDates on save { create; update; }
  validation validateCustomer on save { field CustomerId; }

  action ( features : instance ) bookTravel result [1] $self;
  action ( features : instance ) cancelTravel result [1] $self;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  validation validateFlight on save { create; update; field CarrierId, ConnectionId, FlightDate; }

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating get_instance_features in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.
```

#### Creating get_instance_features in lhc_Travel (implementation)

```abap
METHOD get_instance_features.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Travel
            FIELDS ( TravelId Status )
            WITH CORRESPONDING #( keys )
        RESULT DATA(lt_travel_result).

    result = VALUE #( FOR ls_travel IN lt_travel_result
                      ( %tky = ls_travel-%tky
                        TravelId = if_abap_behv=>fc-f-read_only
                        %features-%action-cancelTravel = COND #( WHEN ls_travel-status = 'X'
                                                                    THEN if_abap_behv=>fc-o-disabled
                                                                 ELSE if_abap_behv=>fc-o-enabled )
                        %features-%action-bookTravel = COND #( WHEN ls_travel-status = 'N' OR ls_travel-status = 'P'
                                                                    THEN if_abap_behv=>fc-o-enabled
                                                                 ELSE if_abap_behv=>fc-o-disabled )
                        %delete = COND #( WHEN ls_travel-Status = 'X' OR ls_travel-Status = 'B'
                                                THEN if_abap_behv=>fc-o-disabled
                                                ELSE if_abap_behv=>fc-o-enabled )
    ) ).
  ENDMETHOD.
```

#### Creating bookTravel in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS booktravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~booktravel RESULT result.
```

#### Creating bookTravel in lhc_Travel (implementation)

```abap
METHOD booktravel.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Travel
            FIELDS ( Status )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    DATA: lt_update TYPE TABLE FOR UPDATE ZR_MS_TravelTP.

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      IF <travel>-Status = 'N' OR <travel>-Status = 'P'.
        APPEND VALUE #( %tky   = <travel>-%tky
                        Status = 'B' ) TO lt_update.
      ELSE.
        APPEND VALUE #( %tky = <travel>-%tky ) TO failed-travel.
        APPEND VALUE #( %tky     = <travel>-%tky
                        %msg     = new_message( id       = 'ZCM_MS_Travel'
                                                number   = '004'
                                                v1       = <travel>-Status
                                                severity = if_abap_behv_message=>severity-error )
                        %element-Status = if_abap_behv=>mk-on )
            TO reported-travel.
      ENDIF.
    ENDLOOP.

    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
          ENTITY Travel
              UPDATE FIELDS ( Status )
              WITH lt_update
          REPORTED DATA(update_reported)
          FAILED DATA(update_failed).
    ENDIF.
  ENDMETHOD.
```

#### Creating cancelTravel in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS canceltravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~canceltravel RESULT result.
```

#### Creating cancelTravel in lhc_Travel (implementation)

```abap
METHOD canceltravel.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
    ENTITY Travel
        FIELDS ( Status )
        WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    DATA: lt_update TYPE TABLE FOR UPDATE ZR_MS_TravelTP.

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      IF <travel>-Status <> 'X'.
        APPEND VALUE #( %tky   = <travel>-%tky
                        Status = 'X' ) TO lt_update.
      ELSE.
        APPEND VALUE #( %tky = <travel>-%tky ) TO failed-travel.
        APPEND VALUE #( %tky     = <travel>-%tky
                        %msg     = new_message( id       = 'ZCM_MS_Travel'
                                                number   = '005'
                                                v1       = <travel>-TravelId
                                                severity = if_abap_behv_message=>severity-error )
                        %element-Status = if_abap_behv=>mk-on )
            TO reported-travel.
      ENDIF.
    ENDLOOP.

    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
          ENTITY Travel
              UPDATE FIELDS ( Status )
              WITH lt_update
          REPORTED DATA(update_reported)
          FAILED DATA(update_failed).

      reported = CORRESPONDING #( DEEP reported ).
      failed = CORRESPONDING #( DEEP failed ).
    ENDIF.


    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY travel
            ALL FIELDS WITH
            CORRESPONDING #( keys )
        RESULT DATA(result_travels).

    result = VALUE #( FOR result_travel IN result_travels
                      ( %tky   = result_travel-%tky
                        %param = result_travel ) ).

  ENDMETHOD.
```

#### Adjusting ZC_TravelTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View for Travel'
@Metadata.allowExtensions: true
define root view entity ZC_MS_TravelTP
  provider contract transactional_query
  as projection on ZR_MS_TravelTP
{
      @UI.lineItem: [
          { type: #FOR_ACTION, dataAction: 'bookTravel', label: 'Book Travel', position: 10 },
          { type: #FOR_ACTION, dataAction: 'cancelTravel', label: 'Cancel Travel', position: 20 }
      ]
      @UI.identification: [
          { type: #FOR_ACTION, dataAction: 'bookTravel', label: 'Book Travel', position: 10 },
          { type: #FOR_ACTION, dataAction: 'cancelTravel', label: 'Cancel Travel', position: 20 }
      ]
  key TravelId,
      AgencyId,
      CustomerId,
      BeginDate,
      EndDate,
      BookingFee,
      TotalPrice,
      CurrencyCode,
      Description,
      Status,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      /* Associations */
      _Agency,
      _Booking : redirected to composition child ZC_MS_BookingTP,
      _Currency,
      _Customer,
      _Status
}
```

#### Adjusting ZC_TravelTP

```cds
projection;
strict ( 2 );
use draft;

define behavior for ZC_MS_TravelTP alias Travel
use etag
{
  use create;
  use update;
  use delete;

  use action Activate;
  use action Discard;
  use action Edit;
  use action Prepare;
  use action Resume;

  use action bookTravel;
  use action cancelTravel;

  use association _Booking { create; with draft; }
}

define behavior for ZC_MS_BookingTP alias Booking
use etag
{
  use update;
  use delete;

  use association _Travel { with draft; }
}
```

### Actions

Changes:
- ZR_TravelTP
  - Added action in Booking
- ZBP_R_Travel
  - Added changeCurrency (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))
- ZC_BookingTP (Projection View)
  - Added the action to the Fiori Elements App
- ZC_TravelTP
  - Added the action used in ZR_TravelTP

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete ( features : instance );

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {
    validation validateDates;
    validation validateCustomer;
  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  validation validateDates on save { create; update; }
  validation validateCustomer on save { field CustomerId; }

  action ( features : instance ) bookTravel result [1] $self;
  action ( features : instance ) cancelTravel result [1] $self;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  validation validateFlight on save { create; update; field CarrierId, ConnectionId, FlightDate; }

  action changeCurrency parameter ZMIND2RAP_A_BookChangeCurr result [1] $self;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating changeCurrency in lhc_Booking (definition)

```abap
PRIVATE SECTION.

    METHODS changeCurrency FOR MODIFY
      IMPORTING keys FOR ACTION Booking~changeCurrency RESULT result.
```

#### Creating changeCurrency in lhc_Booking (implementation)

```abap
METHOD changeCurrency.
    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Booking
            FIELDS ( BookingDate FlightPrice CurrencyCode )
        WITH CORRESPONDING #( keys )
    RESULT DATA(bookings).

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
        DATA(target_currency) = keys[ KEY draft %tky = <booking>-%tky ]-%param-Currency.

        IF NOT zcl_mind2rap_helper=>validate_currency( target_currency ).
            APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
            APPEND VALUE #( %tky = <booking>-%tky
                            %action-changecurrency = if_abap_behv=>mk-on
                            %msg = new_message(
                                id = 'ZCM_MS_Travel'
                                number = '007'
                                severity = if_abap_behv_message=>severity-error
                                v1 = target_currency )
                            ) TO reported-booking.
            CONTINUE.
        ENDIF.

        TRY.
            zcl_mind2rap_amdp=>convert_currency(
              EXPORTING
                iv_amount               = <booking>-FlightPrice
                iv_currency_code_source = <booking>-CurrencyCode
                iv_currency_code_target = target_currency
                iv_exchange_rate_date   = <booking>-BookingDate
              IMPORTING
                ev_amount               =  DATA(result_amount)
            ).
        CATCH cx_amdp_execution_failed.
        ENDTRY.

        IF result_amount IS INITIAL.
            APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
            APPEND VALUE #( %tky = <booking>-%tky
                            %action-changecurrency = if_abap_behv=>mk-on
                            %msg = new_message(
                                id = 'ZCM_MS_Travel'
                                number = '008'
                                severity = if_abap_behv_message=>severity-error
                                v1 = target_currency )
                            ) TO reported-booking.
            CONTINUE.
        ENDIF.

        MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
            ENTITY Booking
                UPDATE FIELDS ( FlightPrice CurrencyCode )
                WITH VALUE #( ( %tky = <booking>-%tky
                                CurrencyCode = target_currency
                                FlightPrice = result_amount ) ).

        APPEND VALUE #( %tky = <booking>-%tky
                        %msg = new_message(
                            id = 'ZCM_MS_Travel'
                            number = '010'
                            severity = if_abap_behv_message=>severity-success
                            v1 = target_currency )
                        ) TO reported-booking.

    ENDLOOP.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Booking
            ALL FIELDS WITH
            CORRESPONDING #( keys )
        RESULT bookings.

    result = VALUE #( FOR booking IN bookings
                      ( %tky   = booking-%tky
                        %param = booking ) ).

ENDMETHOD.
```

#### Adjusting ZC_BookingTP

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View for Booking'
@Metadata.allowExtensions: true
define view entity ZC_MS_BookingTP
  as projection on ZR_MS_BookingTP
{
      @UI.lineItem: [
                    { type: #FOR_ACTION, dataAction: 'changeCurrency', label: 'Change Currency', position: 10 }
      ]
      @UI.identification: [
          { type: #FOR_ACTION, dataAction: 'changeCurrency', label: 'Change Currency', position: 10 }
      ]
  key TravelId,
  key BookingId,
      BookingDate,
      BookingStatus,
      CustomerId,
      CarrierId,
      ConnectionId,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      LocalLastChangedAt,
      LastChangedAt,

      /* Associations */
      _Carrier,
      _Currency,
      _Customer,
      _Status,
      _Travel : redirected to parent ZC_MS_TravelTP
}
```

#### Adjusting ZC_TravelTP

```cds
projection;
strict ( 2 );
use draft;

define behavior for ZC_MS_TravelTP alias Travel
use etag
{
  use create;
  use update;
  use delete;

  use action Activate;
  use action Discard;
  use action Edit;
  use action Prepare;
  use action Resume;

  use action bookTravel;
  use action cancelTravel;

  use association _Booking { create; with draft; }
}

define behavior for ZC_MS_BookingTP alias Booking
use etag
{
  use update;
  use delete;

  use action changeCurrency;

  use association _Travel { with draft; }
}
```

## Calling actions via determinations

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have implemented everything necessary for early numbering

### Internal Actions

Changes:
- ZR_TravelTP
  - Added internal action to Travel
- ZBP_R_Travel
  - Added recalculateTotalPrice (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create;
  update;
  delete ( features : instance );

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {
    validation validateDates;
    validation validateCustomer;
  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  validation validateDates on save { create; update; }
  validation validateCustomer on save { field CustomerId; }

  action ( features : instance ) bookTravel result [1] $self;
  action ( features : instance ) cancelTravel result [1] $self;

  internal action recalculateTotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  validation validateFlight on save { create; update; field CarrierId, ConnectionId, FlightDate; }

  action changeCurrency parameter ZMIND2RAP_A_BookChangeCurr result [1] $self;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating recalculateTotalPrice in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS recalculatetotalprice FOR MODIFY
      IMPORTING keys FOR ACTION travel~recalculatetotalprice.
```

#### Creating recalculateTotalPrice in lhc_Travel (implementation)

```abap
METHOD recalculateTotalPrice.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelId CurrencyCode BookingFee )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
      ENTITY Travel BY \_Booking
        FIELDS ( BookingId FlightPrice CurrencyCode BookingDate )
      WITH CORRESPONDING #( travels )
      RESULT DATA(bookings).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      DATA(total_price) = <travel>-BookingFee.
      DATA(target_currency) = <travel>-CurrencyCode.

      LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>)
           WHERE TravelId = <travel>-TravelId.

        IF <booking>-CurrencyCode <> target_currency.
          zcl_mind2rap_amdp=>convert_currency(
           EXPORTING
                  iv_amount               = <booking>-FlightPrice
                  iv_currency_code_source = <booking>-CurrencyCode
                  iv_currency_code_target = target_currency
                  iv_exchange_rate_date   = <booking>-BookingDate
                IMPORTING
                  ev_amount               =  DATA(converted_price)
              ).

          total_price += converted_price.
        ELSE.
          total_price += <booking>-FlightPrice.
        ENDIF.
      ENDLOOP.

      MODIFY ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Travel
          UPDATE FIELDS ( TotalPrice )
          WITH VALUE #( ( %tky = <travel>-%tky
                          TotalPrice = total_price ) ).
    ENDLOOP.

  ENDMETHOD.
```

## Authorization Checks

Preconditions for this step:
- Have ZR_TravelTP and ZR_BookingTP defined (Root and Child View Entities)
- Have implemented everything necessary for early numbering

### Instance based Authorization Checks

Changes:
- ZR_TravelTP
  - Added precheck to create in Travel
- ZBP_R_Travel
  - Added get_instance_authorizations and precheck_create (can be added via Quick Fix in ZR_TravelTP directly into the correct class(es))

#### Adjusting ZR_TravelTP

```cds
managed implementation in class ZBP_R_MS_TravelTP unique;
strict ( 2 );
with draft;

define behavior for ZR_MS_TravelTP alias Travel
early numbering
persistent table zmind2_travel
draft table zms_d_travel
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  create ( precheck );
  update;
  delete ( features : instance );

  draft action Activate optimized;
  draft action Discard;
  draft action Edit;
  draft action Resume;
  draft determine action Prepare
  {
    validation validateDates;
    validation validateCustomer;
  }

  field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
  field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedAt, LastChangedBy, LocalLastChangedAt, TotalPrice;

  determination setStatusOnCreate on modify { create; }

  validation validateDates on save { create; update; }
  validation validateCustomer on save { field CustomerId; }

  action ( features : instance ) bookTravel result [1] $self;
  action ( features : instance ) cancelTravel result [1] $self;

  internal action recalculateTotalPrice;

  mapping for zmind2_travel corresponding
    {
      AgencyId           = agency_id;
      BeginDate          = begin_date;
      BookingFee         = booking_fee;
      CreatedAt          = createdat;
      CreatedBy          = createdby;
      CurrencyCode       = currency_code;
      CustomerId         = customer_id;
      Description        = description;
      EndDate            = end_date;
      LastChangedAt      = lastchangedat;
      LastChangedBy      = lastchangedby;
      LocalLastChangedAt = locallastchangedat;
      Status             = status;
      TotalPrice         = total_price;
      TravelId           = travel_id;
    }
  association _Booking { create; with draft; }
}

define behavior for ZR_MS_BookingTP alias Booking
early numbering
persistent table zmind2_booking
draft table zms_d_booking
lock dependent by _Travel

authorization dependent by _Travel
etag master LocalLastChangedAt
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId, BookingStatus, LocalLastChangedAt;
  field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
  field ( mandatory ) CarrierId, ConnectionId, FlightDate;
  field ( readonly : update ) BookingDate;

  determination setBookingStatus on modify { create; }
  determination setBookingDate on modify { create; }

  validation validateFlight on save { create; update; field CarrierId, ConnectionId, FlightDate; }

  action changeCurrency parameter ZMIND2RAP_A_BookChangeCurr result [1] $self;

  mapping for zmind2_booking corresponding
    {
      BookingDate        = booking_date;
      BookingId          = booking_id;
      BookingStatus      = booking_status;
      ConnectionId       = connection_id;
      CarrierId          = carrier_id;
      CurrencyCode       = currency_code;
      FlightDate         = flight_date;
      FlightPrice        = flight_price;
      LocalLastChangedAt = local_last_changed_at;
      TravelId           = travel_id;
    }
  association _Travel { with draft; }
}
```

#### Creating get_instance_authorizations in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.
```

#### Creating get_instance_authorizations in lhc_Travel (implementation)

```abap
METHOD get_instance_authorizations.

    DATA: update_requested TYPE abap_bool,
          delete_requested TYPE abap_bool.

    READ ENTITIES OF ZR_MS_TravelTP IN LOCAL MODE
        ENTITY Travel
            FIELDS ( AgencyId )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels)
        FAILED failed.

    CHECK travels IS NOT INITIAL.

    update_requested = COND #( WHEN requested_authorizations-%update = if_abap_behv=>mk-on THEN abap_true
                               WHEN requested_authorizations-%action-Edit = if_abap_behv=>mk-on THEN abap_true
                               ELSE abap_false ).
    delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on THEN abap_true
                               ELSE abap_false ).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<agency>) GROUP BY <agency>-AgencyId.
      DATA(update_authorized) = COND #( WHEN update_requested = abap_true
                                          THEN zcl_mind2rap_helper=>is_granted( agency = <agency>-AgencyId )
                                        ELSE abap_false ).

      DATA(delete_authorized) = COND #( WHEN delete_requested = abap_true
                                          THEN zcl_mind2rap_helper=>is_granted( agency = <agency>-AgencyId  )
                                        ELSE abap_false ).

      LOOP AT GROUP <agency> ASSIGNING FIELD-SYMBOL(<travel>).
        IF ( update_requested = abap_true AND update_authorized = abap_false ) OR
           ( delete_requested = abap_true AND delete_authorized = abap_false ).
          APPEND VALUE #( %tky = <travel>-%tky
                          %msg = new_message(
                              id = 'ZCM_MS_Travel'
                              number = '009'
                              severity = if_abap_behv_message=>severity-error
                              v1 = <travel>-TravelId
                          ) ) TO reported-travel.
        ENDIF.

        APPEND VALUE #( LET update_auth = COND #( WHEN update_authorized = abap_true THEN if_abap_behv=>auth-allowed
                                                  ELSE if_abap_behv=>auth-unauthorized )
                            delete_auth = COND #( WHEN delete_authorized = abap_true THEN if_abap_behv=>auth-allowed
                                                  ELSE if_abap_behv=>auth-unauthorized )
                        IN
                            %tky = <travel>-%tky
                            %update = update_auth
                            %delete = delete_auth
                            %action-Edit = update_auth ) TO result.
      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.
```

#### Creating precheck_create in lhc_Travel (definition)

```abap
PRIVATE SECTION.

    METHODS precheck_create FOR PRECHECK
      IMPORTING entities FOR CREATE travel.
```

#### Creating precheck_create in lhc_Travel (implementation)

```abap
METHOD precheck_create.
  LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).
    DATA(is_authorized) = zcl_mind2rap_helper=>is_granted(
      agency = <entity>-AgencyId
    ).

    IF is_authorized = abap_false.
      APPEND VALUE #( %key        = <entity>-%key ) TO failed-travel.
      APPEND VALUE #( %key        = <entity>-%key
                      %msg     = new_message( id       = 'ZCM_MS_Travel'
                                                number   = '009'
                                                v1 = <entity>-AgencyId
                                                severity = if_abap_behv_message=>severity-error )
                        %element-CustomerId = if_abap_behv=>mk-on )  TO reported-travel.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
```

## Custom Entities

Preconditions for this step:
- None

Note:
- To see the results, expose the Custom Entity View in the (existing) Service Definition. Afterwards open the Custom Entity in the (existing) Service Binding.

### Defining Custom Entities

#### Defining ZI_CustomEntity

```cds
@EndUserText.label: 'Custom Entity View Example'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_MS_CUSTOM_ENTITY'
define custom entity ZI_MS_CustomEntity
{
      @UI.lineItem:[{ position: 10 }]
  key TravelId   : abap.numc( 8 );
      @UI.lineItem:[{ position: 20 }]
      AgencyId   : abap.numc( 6 );
      @UI.lineItem:[{ position: 30 }]
      CustomerId : abap.numc( 6 );
      @UI.lineItem:[{ position: 40 }]
      BeginDate  : abap.dats;
      @UI.lineItem:[{ position: 50 }]
      EndDate    : abap.dats;
      @UI.lineItem:[{ position: 60 }]
      FullPrice  : abap.dec( 17, 3 );
      @UI.lineItem:[{ position: 70 }]
      Currency   : abap.cuky;
      @UI.lineItem:[{ position: 80 }]
      Status     : abap.char( 1 );
}
```

### Defining a Dummy-Class for Testing

#### Defining ZI_CUSTOM_ENTITY (definition + implementation)

```abap
CLASS zcl_ms_custom_entity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MS_CUSTOM_ENTITY IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    DATA dummy_data TYPE STANDARD TABLE OF ZI_DM_CustomEntity.

    IF io_request->is_data_requested(  ).
        DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
        DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
        DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                  THEN 0 ELSE lv_page_size ).
        dummy_data = VALUE #(
            ( TravelId = 1 AgencyId = 1 CustomerId = 1 BeginDate = '20210901' EndDate = '20211009' FullPrice = 1500 Currency = 'EUR' Status = 'A')
            ( TravelId = 2 AgencyId = 2 CustomerId = 4 BeginDate = '20230603' EndDate = '20230703' FullPrice = 1800 Currency = 'EUR' Status = 'A')
            ( TravelId = 3 AgencyId = 1 CustomerId = 2 BeginDate = '20220201' EndDate = '20220309' FullPrice = 1200 Currency = 'USD' Status = 'A') ).

      ENDIF.
      io_response->set_total_number_of_records( iv_total_number_of_records = 5 ).

      io_response->set_data( dummy_data ).
  ENDMETHOD.
ENDCLASS.
```
