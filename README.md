# Begleitmaterialien zur RAP Schulung

Codebeispiele für die [mindsquare Restful Application Programming Model Schulung](https://mindsquare.de/schulungen/)

## Datenmodell
TODO

## Core Data Services im RAP

### Root View Entities

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'RAP Example: Root View'
define root view entity ZREX_I_Carrier
  as select from ZMIND2E_I_Carrier
  composition [0..*] of ZREX_I_Connection as _Connection
{
  key AirlineId as CarrierId,
      Name,
      CurrencyCode,
      
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      /* Associations */
      _Connection
}
```

### Kompositionen

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'RAP Example: Child View'
define view entity ZREX_I_Connection
  as select from ZMIND2E_I_Connection
  association to parent ZREX_I_Carrier as _Carrier on $projection.CarrierId = _Carrier.CarrierId
  composition [0..*] of ZREX_I_Flight  as _Flight
{
  key AirlineId as CarrierId,
  key ConnectionId,
      DepartureAirport,
      DestinationAirport,
      DepartureTime,
      ArrivalTime,
      Distance,
      DistanceUnit,
      LocalLastChangedAt,

      /* Associations */
      _Carrier,
      _Flight
}
```

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'RAP Example: Grant Child View'
define view entity ZREX_I_Flight
  as select from ZMIND2E_I_Flight
  association     to parent ZREX_I_Connection as _Connection on  $projection.CarrierId    = _Connection.CarrierId
                                                             and $projection.ConnectionID = _Connection.ConnectionId
  association [1] to ZREX_I_Carrier           as _Carrier    on  $projection.CarrierId = _Carrier.CarrierId
{
  key AirlineID as CarrierId,
  key ConnectionID,
  key FlightDate,
      Price,
      CurrencyCode,
      PlaneType,
      MaximumSeats,
      OccupiedSeats,

      LocalLastChangedAt,

      /* Associations */
      _Carrier,
      _Connection
}
```

### Annotationen

```cds
define view entity ZMIND2E_I_Flight
  as select from zmind2_flight
  association [0..1] to I_Currency as _Currency on $projection.CurrencyCode = _Currency.Currency
{
  key carrier_id            as AirlineID,
  key connection_id         as ConnectionID,
  key flight_date           as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,

      @ObjectModel.foreignKey.association: '_Currency'
      currency_code         as CurrencyCode,

      // Weitere Felder

      // Assoziation für Fremdschlüsselbeziehung
      _Currency
}
```

```cds
@ObjectModel.representativeKey: 'Currency'
define view I_Currency 
  as select from tcurc
{
    key waers as Currency
}
```

### Abstract CDS Entity

```cds
@EndUserText.label: 'Parameter for Action Book Travel'
define abstract entity ZMIND2RAP_A_BookTravel
{
    @EndUserText.label: 'book flights, too?'
    @EndUserText.quickInfo: 'Should all flights of the selected travels be booked as well?'
    @Consumption.defaultValue: 'X'
    bookFlights : abap_boolean;   
}
```

### Custom CDS Entity

TODO

## Implementierungsarten

### Managed Scenario

```abap
managed;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
...
```

#### Managed Sencario with additional save

Zusätzliche Speichersequenz für alle CDS Entitäten:
```abap
managed with additional save;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
...
```

Zusätzliche Speichersequenz für eine CDS Entität:
```abap
managed;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
with additional save
...
```

#### Managed Scenario with unmanaged save

Unmanaged Speichersequenz für alle CDS Entitäten:
```abap
managed with unmanaged save;

define behavior for ZREX_I_Carrier alias Carrier
...
```

Unmanmaged Speichersequenz für eine CDS Entität:
```abap
managed;

define behavior for ZREX_I_Carrier alias Carrier
with unmanged save
...
```

### Unmanaged Scenario

```abap
unmanaged;

define behavior for ZREX_I_Carrier alias Carrier
...
```

## Einführung RAP

### Behavior Definition

```abap
managed implementation in class zbp_rex_i_carrier unique;
strict ( 2 );
extensible;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
authorization master ( instance )
etag master LocalLastChangedAt
extensible
{
  create;
  update;
  delete;

  association _Connection { create; }
}
```

### Standardaktionen

#### Create, Update & Delete

```abap
define behavior for ZREX_I_Carrier alias Carrier
...
{
  create;
  update;
  delete;
}
```

#### Read per Assoziation

```abap
define behavior for ZREX_I_Carrier alias Carrier
...
{
  ...
  // Lesen per Assoziation
  association _Connection { }
}

define behavior for ZREX_I_Connection alias Connection
...
{
  ...
  // Lesen per Assoziation
  association _Carrier { }
}
```

#### Create per Assoziation

```abap
define behavior for ZREX_I_Carrier alias Carrier
...
{
  ...
  // Lesen und Anlegen per Assoziation
  association _Connection { create; }
}

define behavior for ZREX_I_Connection alias Connection
...
{
  ...
  // Lesen per Assoziation
  association _Carrier { }
}
```

### Feldmapping

```abap
define behavior for ZREX_I_Carrier alias Carrier
...
{
  ...
   mapping for zmind2_carrier corresponding extensible
  {
    CarrierId = carrier_id;
    Name = name;
    CurrencyCode = currency_code;
    CreatedBy = local_created_by;
    CreatedAt = local_created_at;
    LastChangedBy = local_last_changed_by;
    LastChangedAt = last_changed_at;
    LocalLastChangedAt = local_last_changed_at;
  }
  ...
}
```

#### Nutzung in ABAP

Zuweisung CDS Entität als Quelle:
```abap
data ls_po type bapimepoheader.
ls_po = corresponding #( po_entity mapping from entity ).
```

Zuweisung CDS Entität als Ziel:
```abap
data ls_po type bapimepoheader.
data ls_po_entity type zi_rap_purchaseorder_m.
ls_po_entity = corresponding #( ls_po mapping to entity ).
```

### Feature Control

#### Felder: Statische Feature Control

#### Operationen: Statische Feature Control

#### Dynamische Feature Control

### Projection View Entity

```cds
@EndUserText.label: 'RAP Example: Projection Root View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

define root view entity ZREX_C_Carrier
  provider contract transactional_query
  as projection on ZREX_I_Carrier
{
  key CarrierId,
      Name,
      
      @ObjectModel.text.element: ['CurrencyName']
      CurrencyCode,
      @Semantics.text: true
      _Currency._Text.CurrencyName : localized,
      
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,
      LocalLastChangedAt,

      /* Associations */
      _Connection : redirected to composition child ZREX_C_Connection,
      _Currency
}
```

```cds
@EndUserText.label: 'RAP Example: Projection Child View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZREX_C_Connection
  as projection on ZREX_I_Connection
{
  key CarrierId,
  key ConnectionId,
      DepartureAirport,
      DestinationAirport,
      DepartureTime,
      ArrivalTime,
      Distance,
      DistanceUnit,
      LocalLastChangedAt,

      /* Associations */
      _Carrier : redirected to parent ZREX_C_Carrier,
      _Flight  : redirected to composition child ZREX_C_Flight,
      _DepartureAirport,
      _DestinationAirport
}
```

```cds
@EndUserText.label: 'RAP Example: Projection Grant Child View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZREX_C_Flight
  as projection on ZREX_I_Flight
{
  key CarrierId,
  key ConnectionID,
  key FlightDate,
      Price,
      CurrencyCode,
      PlaneType,
      MaximumSeats,
      OccupiedSeats,
      LocalLastChangedAt,

      /* Associations */
      _Carrier    : redirected to ZREX_C_Carrier,
      _Connection : redirected to parent ZREX_C_Connection,
      _Currency
}
```

### Behavior Definition Projektion

```abap
projection;
strict ( 2 );

define behavior for ZREX_C_Carrier alias Carrier
{
  use create;
  use update;
  use delete;

  use association _Connection { create; }
}
```

### Service Definition

```cds
@EndUserText.label: 'RAP Example: Carrier'
define service ZREX_Carrier {
  expose ZREX_C_Carrier;
  expose ZREX_C_Connection;
  expose ZREX_C_Flight;
  expose ZMIND2E_I_Airport as Airport;
  expose I_Country as Country;
  expose I_CountryVH as CountryVH;
  expose I_Currency as Currency;
  expose I_CurrencyStdVH as CurrencyVH;
}
```

### Metadatenerweiterung

Zu erweiternde CDS View Entity:
```cds
@Metadata.allowExtensions: true

define view entity ZREX_C_Carrier
  as select from ZREX_I_Carrier
{
  key CarrierId
  ...
}
```

CDS View Entity Metadatenerweiterung
```cds
@Metadata.layer: #PARTNER

@Search.searchable: true

annotate entity ZREX_C_Carrier with
{
  
  @UI.lineItem: [{ position: 10 }]
  CarrierId;

  ...
}
```

## EML

### Interner Zugriff

```abap
READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
    ENTITY travel
        FIELDS ( travel_id
                 agency_id
                 customer_id
                 booking_fee
                 total_price
                 currency_code )
        WITH CORRESPONDING #( keys )
    RESULT DATA(lt_read_result)
    FAILED failed
    REPORTED reported.
```

### EML Read

```abap
READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
    ENTITY travel
        FIELDS ( travel_id
                 agency_id
                 customer_id
                 booking_fee
                 total_price
                 currency_code )
        WITH CORRESPONDING #( keys )
    RESULT DATA(lt_read_result)
    FAILED failed
    REPORTED reported.
```

### EML Read per Assoziation

```abap
READ ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
    ENTITY Travel BY \_Booking
        FIELDS ( FlightPrice CurrencyCode )
        WITH VALUE #( ( %tky = <fs_travel>-%tky ) )
    RESULT DATA(lt_booking).
```

### EML Create

```abap
MODIFY ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
    ENTITY travel
        CREATE FIELDS ( travel_id
                        agency_id
                        customer_id
                        begin_date
                        end_date
                        booking_fee
                        total_price
                        currency_code
                        description
                        overall_status )
        WITH lt_create
    MAPPED mapped
    FAILED failed
    REPORTED reported.
```

### EML Deep Create

```abap
MODIFY ENTITIES OF /DMO/I_Travel_D
    ENTITY Travel
        CREATE 
            FIELDS ( CustomerID 
                        AgencyID 
                        BeginDate 
                        EndDate 
                        Description ) 
            WITH create " Variable vom TYPE TABLE FOR CREATE /DMO/I_TRAVEL_D
        CREATE BY \_Booking
            FIELDS ( CustomerID 
                     AirlineID 
                     ConnectionID
                     FlightDate ) 
            WITH VALUE #( (
                %cid_ref = 'create_travel'
                %target = VALUE #( 
                (
                    %cid = 'create_booking_1'
                    CustomerID = '1'
                    AirlineID = flight-AirlineID
                    ConnectionID = flight-ConnectionID
                    FlightDate = flight-FlightDate )
                ( 
                    %cid = 'create_booking_2'
                    CustomerID = '1'
                    AirlineID = flight-AirlineID
                    ConnectionID = flight-ConnectionID
                    FlightDate = flight-FlightDate) ) ) )
    MAPPED DATA(mapped)
    REPORTED DATA(reported)
    FAILED DATA(failed).
```

### EML Update

```abap
MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
    ENTITY Travel
        UPDATE 
            FIELDS ( OverallStatus )
            WITH VALUE #( FOR key IN keys (
                          %tky = key-%tky
                          OverallStatus = travel_status-accepted ) )
    REPORTED DATA(reported)
    FAILED DATA(failed).
```

### EML Delete

```abap
MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
    ENTITY Travel
        DELETE FROM VALUE #( ( TravelUUID = lv_travel_id ) )
    REPORTED DATA(reported)
    FAILED DATA(failed).
```

### EML Aktionen ausführen

```abap
MODIFY ENTITIES OF /DMO/I_Travel_D IN LOCAL MODE
    ENTITY Travel
        EXECUTE reCalcTotalPrice
        FROM CORRESPONDING #( keys )
    REPORTED DATA(lt_reported).
```

### EML Verschiedene Operationen in einer Modifiy Anweisung

### EML Commit & Rollback

```abap
COMMIT ENTITIES.

ROLLBACK ENTITIES.
```

## Behavior Pool

### BP für Verhaltensdefinition

```abap
managed implementation in class zbp_rex_i_carrier unique;
strict ( 2 );

define behavior for ZREX_I_Carrier alias Carrier
...
```

### BP für CDS Entität

```abap
managed;
strict ( 2 );

define behavior for ZREX_I_Carrier alias Carrier
implementation in class zbp_rex_i_carrier unique
...

define behavior for ZREX_I_Connection alias Connection
implementation in class zbp_rex_i_connection unique
...
```

### BP für Implementierungsgruppen

TODO

### Phasen des Programmflusses

#### Methodendefinition

```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
    PRIVATE SECTION.
        TYPES:
            tt_travel_update TYPE TABLE
                FOR UPDATE /dmo/i_travel_u.
        
        METHODS:
            create_travel FOR MODIFY
                IMPORTING it_travel_create FOR CREATE travel,
            update_travel FOR MODIFY
                IMPORTING it_travel_update FOR UPDATE travel,
            delete_travel FOR MODIFY
                IMPORTING it_travel_delete FOR DELETE travel,
            read_travel FOR READ
                IMPORTING it_travel FOR READ travel
                RESULT et_travel,
            create_booking_ba FOR MODIFY
                IMPORTING it_booking_create_ba
                FOR CREATE travel\_booking,
            read_booking_ba FOR READ
                IMPORTING it_travel FOR READ travel\_Booking
                FULL iv_full_requested
                RESULT et_booking
                LINK et_link_table,
            lock FOR LOCK
                IMPORTING it_travel_lock FOR LOCK travel,
            set_travel_status FOR MODIFY
                IMPORTING it_travel_set_status_booked
                FOR ACTION travel~set_status_booked
                RESULT et_travel_set_status_booked,
            get_features FOR FEATURES
                IMPORTING keys REQUEST requested_features
                FOR travel
                RESULT result.
ENDCLASS.
```

### Messages

## Nummernvergabe

### Frühe, interne Nummernvergabe

#### Nummernvergabe mit Nummernkreis

#### Nummernvergabe durch Hochzählen

### Späte Nummernvergabe

## Ermittlungen

### Ermittlungen definieren

### Ermittlungen implementieren

## Validierung

### Validierungen definieren

### Validierungen implementieren

### Vorprüfungen

## Aktionen

### statische Aktionen

### Factory Aktionen

### Eingabeparameter

### Rückgabeparameter

### Aktionen implementieren

### Aktionen: Dynamische Feature Control

### Aktionen: Fiori Elements

## Berechtigungen

### Lesende Berechtigungen

### Authorization Master

### Authorization Dependent

### Global Authorization

### Instance Authorization

### Precheck

## Sperren

### Pessimistische Sperren

### Optimistische Sperren

## Draft Handling

### Draft Tabellen

### Draft etag Handling

### Draft Assoziationen

### Draft Aktionen


