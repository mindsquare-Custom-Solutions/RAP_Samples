## ZMIND2RAP_C_Travel_UI

```cds
@Metadata.layer: #PARTNER

@UI.headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title.value: 'Description',
    description.value: 'CustomerId'
}

@UI.presentationVariant: [{
    sortOrder: [{ by: 'BeginDate', direction: #ASC }]
}]
annotate view ZMIND2RAP_C_Travel with
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
    id: 'CustomerFieldGroup',
    parentId: 'CustomerCollection',
    targetQualifier: 'Customer',
    label: 'Customer',
    position: 10
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
  @UI.lineItem: [{ position: 5 }]
  TravelId;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 20 }]
  @Consumption.valueHelpDefinition: [{
      entity: {
          name: 'ZI_I_MSQ_AgencyVH',
          element: 'AgencyId'
      },
      useForValidation: true
  }]
  AgencyId;

  @UI.fieldGroup: [{ qualifier: 'Customer', position: 10 }]
  @UI.lineItem: [{ position: 30 }]
  @Consumption.valueHelpDefinition: [{
      entity: {
          name: 'ZI_I_MSQ_CustomerVH',
          element: 'CustomerId'
      },
      useForValidation: true
  }]
  @UI.textArrangement: #TEXT_FIRST
  CustomerId;

  @UI.fieldGroup: [{ qualifier: 'Dates', position: 10 }]
  @UI.lineItem: [{ position: 10 }]
  BeginDate;

  @UI.fieldGroup: [{ qualifier: 'Dates', position: 20 }]
  @UI.lineItem: [{ position: 20 }]
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
  @UI.lineItem: [{ position: 40 }]
  TotalPrice;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 30 }]
  @UI.lineItem: [{ position: 60, emphasized: true }]
  Description;

  @UI.fieldGroup: [{ qualifier: 'Details', position: 40 }]
  @UI.lineItem: [{ position: 50 }]
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

## ZMIND2RAP_C_Booking_UI

```cds
@Metadata.layer: #CORE

@UI.headerInfo: {
    typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title.value: 'BookingDate',
    description.value: 'CustomerId'
}

@UI.presentationVariant: [{ 
    sortOrder: [{ by: 'FlightDate', direction: #ASC }]
}]

annotate view ZMIND2RAP_C_Booking with
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
    entity: { name: 'ZI_I_MSQ_FlightVH', element: 'CarrierId' },
    additionalBinding: [
      { element: 'ConnectionID', localElement: 'ConnectionId', usage: #RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  CarrierId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 40 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 40 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZI_I_MSQ_FlightVH', element: 'ConnectionID' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  ConnectionId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 50 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 50 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZI_I_MSQ_FlightVH', element: 'FlightDate' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'ConnectionID', localElement: 'ConnectionId', usage: #FILTER_AND_RESULT }
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
