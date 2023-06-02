<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

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

#### Annotationen für administrative Felder

```cds
define view entity ZMIND2E_I_Carrier
  as select from zmind2_carrier
{
  key carrier_id            as AirlineId,

      @Semantics.user.createdBy: true
      local_created_by      as CreatedBy,

      @Semantics.systemDateTime.createdAt: true
      local_created_at      as CreatedAt,

      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,

      @Semantics.user.lastChangedBy: true
      local_last_changed_by as LastChangedBy,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt
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

```cds
@EndUserText.label: 'Custom entity for unmanaged travel query'
@ObjectModel.query.implementedBy:'ABAP:/dmo/cl_travel_uq' 

define custom entity /DMO/I_TRAVEL_UQ 
{
  key Travel_ID     : abap.numc( 8 );
      Agency_ID     : abap.numc( 6 );
      Customer_ID   : abap.numc( 6 );
      Begin_Date    : abap.dats;
      End_Date      : abap.dats;
      Booking_Fee   : abap.dec( 17, 3 );
      Total_Price   : abap.dec( 17, 3 );
      Currency_Code : abap.cuky;
      Status        : abap.char( 1 );
      LastChangedAt : timestampl;
}
```

## Implementierungsarten

### Managed Scenario

```cds
managed;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
...
```

#### Managed Sencario with additional save

Zusätzliche Speichersequenz für alle CDS Entitäten:
```cds
managed with additional save;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
...
```

Zusätzliche Speichersequenz für eine CDS Entität:
```cds
managed;

define behavior for ZREX_I_Carrier alias Carrier
persistent table zmind2_carrier
with additional save
...
```

#### Managed Scenario with unmanaged save

Unmanaged Speichersequenz für alle CDS Entitäten:
```cds
managed with unmanaged save;

define behavior for ZREX_I_Carrier alias Carrier
...
```

Unmanmaged Speichersequenz für eine CDS Entität:
```cds
managed;

define behavior for ZREX_I_Carrier alias Carrier
with unmanged save
...
```

### Unmanaged Scenario

```cds
unmanaged;

define behavior for ZREX_I_Carrier alias Carrier
...
```

## Einführung RAP

### Behavior Definition

```cds
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

```cds
define behavior for ZREX_I_Carrier alias Carrier
...
{
  create;
  update;
  delete;
}
```

#### Read per Assoziation

```cds
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

```cds
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

```cds
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

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    field ( numbering : managed ) SalesOrderUuid;
    // Schlüssel auf nur lesend setzen
    field ( readonly ) SalesOrderUuid;
    field ( mandatory : create ) SalesOrderType;

    field( mandatory : create, readonly : update ) PersonId;
    ...
```

#### Operationen: Statische Feature Control

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    internal create;
    ...
}
```

#### Dynamische Feature Control

Global:
```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    create ( features : global );
}
```

```abap
CLASS lhc_handler DEFINITION INHERITING FROM cl_abap_behavior_handler.
    PRIVATE SECTION.
    METHODS get_global_features FOR GLOBAL FEATURES
        IMPORTING REQUEST requested_features
        FOR entity RESULT result.
ENDCLASS.

CLASS lhc_handler IMPLEMENTATION.
    METHOD get_global_features.
        result = VALUE #(
            " Feature Control für Aktion
            %features-%action-action_name = COND #( WHEN condition
                                                        THEN if_abap_behv=>fc-o-disabled
                                                    ELSE if_abap_behv=>fc-o-enabled )
            %features-%update = COND #( WHEN condition
                                            THEN if_abap_behv=>fc-o-disabled
                                        ELSE if_abap_behv=>fc-o-enabled )

            " Feature Control für eine create-Operation per Assoziation
            %assoc-_Assoc = COND #( WHEN condition
                                        THEN if_abap_behv=>fc-o-disabled
                                    ELSE if_abap_behv=>fc-o-enabled )
        ).
    ENDMETHOD.
ENDCLASS.
```

Instanzbasiert:
```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    create ( features : instance );
}
```

```abap
CLASS lhc_handler DEFINITION INHERITING FROM cl_abap_behavior_handler.
    PRIVATE SECTION.
    METHODS get_features FOR INSTANCE FEATURES
        IMPORTING keys
        FOR entity RESULT result.
ENDCLASS.

CLASS lhc_handler IMPLEMENTATION.
    METHOD get_features.
        READ ENTITIES OF /dmo/i_travel_m IN LOCAL MODE
            ENTITY travel
                FIELDS ( travel_id overall_status )
                WITH CORRESPONDING #( keys )
            RESULT DATA(lt_travel_result).

        result = VALUE #( FOR ls_travel IN lt_travel_result
                          ( %key = ls_travel-%key
                            
                            %field-travel_id = if_abap_behv=>fc-f-read_only
                            
                            %features-%action-rejectTravel = COND #( WHEN ls_travel-overall_status = 'X'
                                                                        THEN if_abap_behv=>fc-o-disabled
                                                                     ELSE if_abap_behv=>fc-o-enabled )
                            %features-%action-acceptTravel = COND #( WHEN ls_travel-overall_status = 'A'
                                                                        THEN if_abap_behv=>fc-o-disabled
                                                                     ELSE if_abap_behv=>fc-o-enabled )
                            
                            %assoc-_Booking = COND #( WHEN ls_travel-overall_status = 'X'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled )
        ) ).
    ENDMETHOD.
ENDCLASS.
```

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

```cds
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

```cds
managed implementation in class zbp_rex_i_carrier unique;
strict ( 2 );

define behavior for ZREX_I_Carrier alias Carrier
...
```

### BP für CDS Entität

```cds
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

```abap
APPEND VALUE #( %tky = <carrier>-%tky
                %msg = new_message(
                    id = 'ZMC_REX_CARRIER'
                    number = '001'
                    severity = if_abap_behv_message=>severity-error )
                    %element-CarrierId = if_abap_behv=>mk-on 
                ) TO reported-carrier.
```

## Nummernvergabe

### Frühe, interne Nummernvergabe

```cds
define behavior for ZI_SalesOrder alias SalesOrder
early numbering
...
{
    ...
```

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    field ( numbering : managed ) SalesOrderUuid;
    ...
}
```

#### Nummernvergabe mit Nummernkreis

```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler

  PRIVATE SECTION.
    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE travel.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD earlynumbering_create.

    DATA:
      entity        TYPE STRUCTURE FOR CREATE /DMO/I_Travel_M,
      travel_id_max TYPE /dmo/travel_id.

    " Ensure Travel ID is not set yet (idempotent)- must be checked when BO is draft-enabled
    LOOP AT entities INTO entity WHERE travel_id IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-travel.
    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    DELETE entities_wo_travelid WHERE travel_id IS NOT INITIAL.

    " Get Numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            nr_range_nr       = '01'
            object            = '/DMO/TRV_M'
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
                           %msg = lx_number_ranges
                        ) TO reported-travel.
          APPEND VALUE #(  %cid = entity-%cid
                           %key = entity-%key
                        ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDTRY.

    CASE number_range_return_code.
      WHEN '1'.
        " 1 - the returned number is in a critical range (specified under “percentage warning” in the object definition)
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = NEW /dmo/cm_flight_messages(
                                      textid = /dmo/cm_flight_messages=>number_range_depleted
                                      severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-travel.
        ENDLOOP.

      WHEN '2' OR '3'.
        " 2 - the last number of the interval was returned
        " 3 - if fewer numbers are available than requested,  the return code is 3
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %msg = NEW /dmo/cm_flight_messages(
                                      textid = /dmo/cm_flight_messages=>not_sufficient_numbers
                                      severity = if_abap_behv_message=>severity-warning )
                        ) TO reported-travel.
          APPEND VALUE #( %cid        = entity-%cid
                          %key        = entity-%key
                          %fail-cause = if_abap_behv=>cause-conflict
                        ) TO failed-travel.
        ENDLOOP.
        EXIT.
    ENDCASE.

    " At this point ALL entities get a number!
    ASSERT number_range_returned_quantity = lines( entities_wo_travelid ).

    travel_id_max = number_range_key - number_range_returned_quantity.

    " Set Travel ID
    LOOP AT entities_wo_travelid INTO entity.
      travel_id_max += 1.
      entity-travel_id = travel_id_max .

      APPEND VALUE #( %cid  = entity-%cid
                      %key  = entity-%key
                    ) TO mapped-travel.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
```

#### Nummernvergabe durch Hochzählen

```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler

  PRIVATE SECTION.
    METHODS earlynumbering_cba_booking FOR NUMBERING
      IMPORTING entities FOR CREATE travel\_booking.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD earlynumbering_cba_booking.
    DATA: max_booking_id TYPE /dmo/booking_id.

    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
      ENTITY travel BY \_booking
        FROM CORRESPONDING #( entities )
        LINK DATA(bookings).

    " Loop over all unique TravelIDs
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel_group>) GROUP BY <travel_group>-travel_id.

      " Get highest booking_id from bookings belonging to travel
      max_booking_id = REDUCE #( INIT max = CONV /dmo/booking_id( '0' )
                                 FOR  booking IN bookings USING KEY entity WHERE ( source-travel_id  = <travel_group>-travel_id )
                                 NEXT max = COND /dmo/booking_id( WHEN booking-target-booking_id > max
                                                                    THEN booking-target-booking_id
                                                                    ELSE max )
                               ).
      " Get highest assigned booking_id from incoming entities
      max_booking_id = REDUCE #( INIT max = max_booking_id
                                 FOR  entity IN entities USING KEY entity WHERE ( travel_id  = <travel_group>-travel_id )
                                 FOR  target IN entity-%target
                                 NEXT max = COND /dmo/booking_id( WHEN   target-booking_id > max
                                                                    THEN target-booking_id
                                                                    ELSE max )
                               ).

      " Loop over all entries in entities with the same TravelID
      LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel>) USING KEY entity WHERE travel_id = <travel_group>-travel_id.

        " Assign new booking-ids if not already assigned
        LOOP AT <travel>-%target ASSIGNING FIELD-SYMBOL(<booking_wo_numbers>).
          APPEND CORRESPONDING #( <booking_wo_numbers> ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
          IF <booking_wo_numbers>-booking_id IS INITIAL.
            max_booking_id += 10 .
            <mapped_booking>-booking_id = max_booking_id .
          ENDIF.
        ENDLOOP.

      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
```

### Späte Nummernvergabe

```cds
define behavior for ZI_SalesOrder alias SalesOrder
late numbering
...
{
    // Schlüsselfeld auf »nur lesend« setzen
    field ( readonly ) SalesOrderId;
    ...
}
```

## Ermittlungen

### Ermittlungen definieren

```cds
define behavior for ZI_SalesOrderItem alias Item
...
{
    ...
    determination calcTotalAmount on modify { delete; field ItemAmount }
}
```

### Ermittlungen implementieren

```cds
define behavior for ZMIND2RAP_I_Booking alias Booking
...
{
  ...
  determination setBookingDate on modify { create; }
}
```

```abap
METHOD setBookingDate.
    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Booking
            FIELDS ( BookingDate )
            WITH CORRESPONDING #( keys )
        RESULT DATA(bookings).

    DELETE bookings WHERE BookingDate IS NOT INITIAL.
    CHECK bookings IS NOT INITIAL.

    MODIFY ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Booking
            UPDATE FIELDS ( BookingDate )
            WITH VALUE #( FOR booking IN bookings
                          ( %tky = booking-%tky
                            BookingDate = cl_abap_context_info=>get_system_date( ) ) ).
ENDMETHOD.
```

## Validierung

### Validierungen definieren

Operationen als Auslösebedingung:
```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    validation validateOnChange on save { create; update; }
}
```

Felder als Ausläsebedingung:
```cds
define behavior for ZI_SalesOrderItem alias Item
...
{
    ...
    validation validateMaterial on save { field MaterialId; }
}
```

Kombination aus Operation und Feld:
```cds
define behavior for ZI_SalesOrderItem alias Item
...
{
    ...
    field ( mandatory ) MaterialId;
    validation validateMaterial on save { create; field MaterialId; }
}
```

### Validierungen implementieren

```abap
METHOD validate_agency.
    " Read relevant travel instance data
    READ ENTITIES OF /DMO/I_Travel_M IN LOCAL MODE
        ENTITY travel
            FIELDS ( agency_id )
            WITH CORRESPONDING #(  keys )
        RESULT DATA(travels).

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    agencies = CORRESPONDING #(  travels DISCARDING DUPLICATES MAPPING agency_id = agency_id EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.
    IF  agencies IS NOT INITIAL.

      " check if agency ID exist
      SELECT FROM /dmo/agency FIELDS agency_id
        FOR ALL ENTRIES IN @agencies
        WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(agencies_db).
    ENDIF.

    " Raise msg for non existing and initial agency id
    LOOP AT travels INTO DATA(travel).
      IF travel-agency_id IS INITIAL
         OR NOT line_exists( agencies_db[ agency_id = travel-agency_id ] ).

        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #(  %tky = travel-%tky
                         %msg      = NEW /dmo/cm_flight_messages(
                                          textid    = /dmo/cm_flight_messages=>agency_unkown
                                          agency_id = travel-agency_id
                                          severity  = if_abap_behv_message=>severity-error )
                         %element-agency_id = if_abap_behv=>mk-on
                      ) TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
```

### Vorprüfungen

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    create ( precheck );
    ...
}
```

```abap
CLASS lhc_salesorder DEFINITION INHERITING FROM cl_abap_behavior_handler.
    ...
    METHODS testCheck for PRECHECK
        IMPORTING keys FOR ACTION SalesOrder~test.
ENDCLASS.

CLASS lhc_salesorder IMPLEMENTATION.
    ...
    METHOD testcheck.
        READ ENTITIES OF ...
        IF ...
        ...
    ENDMETHOD.
ENDCLASS.
```

## Aktionen

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    action cancel;
}
```

### statische Aktionen

```cds
define behavior for ZI_Address alias Address
...
{
    ...
    static action markDuplicates;
}
```

### Factory Aktionen

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    factory action copy [1];
}
```

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    factory action deepCopy parameter zrap_s_so_copy_ops [1];
}
```

### Eingabeparameter

```cds
define behavior for ZI_SalesOrder alias SalesOrder
...
{
    ...
    action cancel parameter ZRAP_A_CancellationOpts;
}
```

### Rückgabeparameter

```cds
define behavior for ZI_Address alias Address
...
{
    ...
    action setAsDefault result [1] $self;
}
```

### Aktionen implementieren

```cds
define behavior for ZMIND2RAP_I_Travel alias Travel
...
{
    ...
    action ( features : instance ) cancelTravel result [1] $self;
}
```

```abap
METHOD cancelTravel.
    MODIFY ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
           ENTITY Travel
              UPDATE FIELDS ( Status )
              WITH VALUE #( FOR key IN keys
                            ( %tky = key-%tky
                              status = 'X' ) ).

    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
      ENTITY travel
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                      ( %tky = travel-%tky
                        %param = travel ) ).
ENDMETHOD.
```

### Aktionen: Dynamische Feature Control

```cds
define behavior for ZMIND2RAP_I_Travel alias Travel
...
{
    ...
    action ( features : instance ) bookTravel parameter ZMIND2RAP_A_BookTravel result [1] $self;
    action ( features : instance ) cancelTravel result [1] $self;
}
```

```abap
METHOD get_features.
        READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
            ENTITY travel
                FIELDS ( travel_id overall_status )
                WITH CORRESPONDING #( keys )
            RESULT DATA(lt_travel_result).

        result = VALUE #( FOR ls_travel IN lt_travel_result
                          ( %tky = ls_travel-%tky
                            
                            %field-travel_id = if_abap_behv=>fc-f-read_only
                            
                            %features-%action-rejectTravel = COND #( WHEN ls_travel-overall_status = 'X'
                                                                        THEN if_abap_behv=>fc-o-disabled
                                                                     ELSE if_abap_behv=>fc-o-enabled )
                            %features-%action-acceptTravel = COND #( WHEN ls_travel-overall_status = 'A'
                                                                        THEN if_abap_behv=>fc-o-disabled
                                                                     ELSE if_abap_behv=>fc-o-enabled )
                            
                            %assoc-_Booking = COND #( WHEN ls_travel-overall_status = 'X'
                                                        THEN if_abap_behv=>fc-o-disabled
                                                      ELSE if_abap_behv=>fc-o-enabled )
        ) ).
```

### Aktionen: Fiori Elements

```cds
define root view entity ZMIND2RAP_C_Travel
  provider contract transactional_query
  as projection on ZMIND2RAP_I_Travel
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
  ...
```

## Funktionen

```cds
define behavior for DEMO_CDS_FUNCTION_1 alias PurchaseDocument
{
  ...

  // instance function
  function getDetails result [0..*] $self;

  // static function
  static function calculateTotal result [1] demo_sales_total_price;

  //function with input parameter
  function calculateDiscount parameter DEMO_CDS_DEDUCT_DISCOUNT
                             result [1] $self;
  ...
}
```

```abap
CLASS lhc_PurchaseDocument
DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS getDetails FOR READ
      IMPORTING keys FOR FUNCTION PurchaseDocument~getDetails
        RESULT result.
    METHODS calculateTotal FOR READ
      IMPORTING keys FOR FUNCTION PurchaseDocument~calculateTotal
        RESULT result.
    METHODS calculateDiscount FOR READ
      IMPORTING keys FOR FUNCTION PurchaseDocument~calculateDiscount
        RESULT result.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR PurchaseDocument RESULT result.
ENDCLASS.

CLASS lhc_PurchaseDocument IMPLEMENTATION.
  METHOD getDetails.
    DATA(lt_keys) = keys.
    CHECK lt_keys IS NOT INITIAL.

    READ ENTITIES OF demo_cds_function_1 IN LOCAL MODE
      ENTITY PurchaseDocument
        FIELDS ( PurchaseDocument Price Status )
        WITH CORRESPONDING #( lt_keys )
      RESULT DATA(lt_item)
      FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).

    LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
     APPEND VALUE #( %tky   = <fs_item>-%tky
                     %param = CORRESPONDING #( <fs_item> ) ) TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateTotal.
    SELECT *
    FROM demo_purch_doc
    WHERE status = 'O'
    INTO TABLE @DATA(lt_db_new_purch).

    READ ENTITIES OF demo_cds_function_1 IN LOCAL MODE
      ENTITY PurchaseDocument
        FIELDS ( Price )
        WITH CORRESPONDING #( lt_db_new_purch )
      RESULT DATA(read_result)
      FAILED failed
      REPORTED reported.

    DATA lv_sum TYPE demo_cds_function_1-Price.
    CLEAR lv_sum.

    LOOP AT read_result ASSIGNING FIELD-SYMBOL(<fs_new_purch>).
      lv_sum += <fs_new_purch>-Price.
    ENDLOOP.

    result = VALUE #( ( %cid    = keys[ 1 ]-%cid
                        %param  = lv_sum )  ).
  ENDMETHOD.

  METHOD calculateDiscount.
    DATA lt_reduced_purch TYPE TABLE FOR FUNCTION RESULT
      demo_cds_function_1\\PurchaseDocument~calculateDiscount.
    DATA lt_update_purch TYPE TABLE FOR UPDATE
      demo_cds_function_1\\PurchaseDocument.
    DATA(lt_keys) = keys.

    LOOP AT lt_keys
    ASSIGNING FIELD-SYMBOL(<fs_key>)
    WHERE %param-discount_percent IS INITIAL
                                  OR %param-discount_percent > 100
                                  OR %param-discount_percent <= 0.

      APPEND VALUE #( %tky = <fs_key>-%tky ) TO failed-purchasedocument.
      APPEND VALUE #( %tky = <fs_key>-%tky
                      %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-error
                      text = 'function failed' )
                      %element-price = if_abap_behv=>mk-on )
                      TO reported-purchasedocument.
      DELETE lt_keys.
    ENDLOOP.

    CHECK lt_keys IS NOT INITIAL.

    "get total price
    READ ENTITIES OF demo_cds_function_1 IN LOCAL MODE
      ENTITY PurchaseDocument
        ALL FIELDS
        WITH CORRESPONDING #( lt_keys )
      RESULT DATA(lt_purc)
      FAILED DATA(read_failed).

    failed = CORRESPONDING #( DEEP read_failed ).

    LOOP AT lt_purc ASSIGNING FIELD-SYMBOL(<fs_purch>).
      DATA lv_percentage TYPE decfloat16.
      DATA(lv_discount_percent) = lt_keys[
      KEY entity  %tky = <fs_purch>-%tky ]-%param-discount_percent.
      lv_percentage =  lv_discount_percent / 100 .
      <fs_purch>-Price = <fs_purch>-price * ( 1 - lv_percentage ) .

      APPEND VALUE #( %tky               = <fs_purch>-%tky
                      %param             = CORRESPONDING #( <fs_purch> ) )
                      TO lt_reduced_purch.
    ENDLOOP.

    result = VALUE #( FOR purchase IN lt_reduced_purch
                          ( %tky   = purchase-%tky
                            %param = purchase-%param ) ).
  ENDMETHOD.
  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
```

## Berechtigungen

### Lesende Berechtigungen

```cds
@EndUserText.label: 'Carrier'
@MappingRole: true
define role ZMIND2E_C_CARRIER {
    grant select on ZMIND2E_C_CARRIER
    where (AirlineId) = aspect pfcg_auth(S_CARRID, CARRID, ACTVT = '03' );
    
}
```

### Authorization Master

```cds
define behavior for /DMO/I_Travel_D alias Travel
...
authorization master ( global, instance )
{
    create;
    update;
    delete;
    action acceptTravel ...;
    ...
}
```

### Authorization Dependent

```cds
define behavior for /DMO/I_Booking_D alias Booking
...
authorization dependent by _Travel
{
    update;
    delete;
    association _Travel { }
}
```

### Global Authorization

```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

...
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR travel RESULT result.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.
  METHOD get_global_authorizations.
    IF requested_authorizations-%create EQ if_abap_behv=>mk-on.
      IF is_create_granted( ) = abap_true.
        result-%create = if_abap_behv=>auth-allowed.
      ELSE.
        result-%create = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /DMO/CM_FLIGHT_MESSAGES(
                                       textid   = /DMO/CM_FLIGHT_MESSAGES=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on 
                      ) TO reported-travel.

      ENDIF.
    ENDIF.

    "Edit is treated like update
    IF requested_authorizations-%update                =  if_abap_behv=>mk-on OR
       requested_authorizations-%action-Edit           =  if_abap_behv=>mk-on.

      IF  is_update_granted( ) = abap_true.
        result-%update                =  if_abap_behv=>auth-allowed.
        result-%action-Edit           =  if_abap_behv=>auth-allowed.

      ELSE.
        result-%update                =  if_abap_behv=>auth-unauthorized.
        result-%action-Edit           =  if_abap_behv=>auth-unauthorized.

        APPEND VALUE #( %msg    = NEW /DMO/CM_FLIGHT_MESSAGES(
                                       textid   = /DMO/CM_FLIGHT_MESSAGES=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on 
                      ) TO reported-travel.

      ENDIF.
    ENDIF.


    IF requested_authorizations-%delete =  if_abap_behv=>mk-on.
      IF is_delete_granted( ) = abap_true.
        result-%delete = if_abap_behv=>auth-allowed.
      ELSE.
        result-%delete = if_abap_behv=>auth-unauthorized.
        APPEND VALUE #( %msg    = NEW /DMO/CM_FLIGHT_MESSAGES(
                                       textid   = /DMO/CM_FLIGHT_MESSAGES=>not_authorized
                                       severity = if_abap_behv_message=>severity-error )
                        %global = if_abap_behv=>mk-on 
                       ) TO reported-travel.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

### Instance Authorization

```abap
CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

...
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.
  METHOD get_instance_authorizations.

    DATA: update_requested TYPE abap_bool,
          delete_requested TYPE abap_bool,
          update_granted   TYPE abap_bool,
          delete_granted   TYPE abap_bool.

    READ ENTITIES OF /DMO/R_Travel_D IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID )
        WITH CORRESPONDING #( keys )
    RESULT DATA(travels)
    FAILED failed.

    CHECK travels IS NOT INITIAL.

    "Select country_code and agency of corresponding persistent travel instance
    "authorization  only checked against instance that have active persistence
    SELECT  FROM /DMO/A_TRAVEL_D AS travel  INNER JOIN /DMO/AGENCY AS agency
          ON travel~agency_id = agency~agency_id
          FIELDS travel~travel_uuid , travel~agency_id, agency~country_code
          FOR ALL ENTRIES IN @travels WHERE travel_uuid EQ @travels-TravelUUID
          INTO  TABLE @DATA(travel_agency_country).


    "edit is treated like update
    update_requested = COND #( WHEN requested_authorizations-%update                = if_abap_behv=>mk-on OR
                                    requested_authorizations-%action-Edit           = if_abap_behv=>mk-on
                               THEN abap_true ELSE abap_false ).

    delete_requested = COND #( WHEN requested_authorizations-%delete                = if_abap_behv=>mk-on
                               THEN abap_true ELSE abap_false ).


    LOOP AT travels INTO DATA(travel).
      "get country_code of agency in corresponding instance on persistent table
      READ TABLE travel_agency_country WITH KEY travel_uuid = travel-TravelUUID
        ASSIGNING FIELD-SYMBOL(<travel_agency_country_code>).

      "Auth check for active instances that have before image on persistent table
      IF sy-subrc = 0.

        "check auth for update
        IF update_requested = abap_true.
          update_granted = is_update_granted( <travel_agency_country_code>-country_code  ).
          IF update_granted = abap_false.
            APPEND VALUE #( %tky = travel-%tky
                            %msg = NEW /DMO/CM_FLIGHT_MESSAGES(
                                                     textid    = /DMO/CM_FLIGHT_MESSAGES=>not_authorized_for_agencyid
                                                     agency_id = travel-AgencyID
                                                     severity  = if_abap_behv_message=>severity-error )
                            %element-AgencyID = if_abap_behv=>mk-on
                           ) TO reported-travel.
          ENDIF.
        ENDIF.

        "check auth for delete
        IF delete_requested = abap_true.
          delete_granted = is_delete_granted( <travel_agency_country_code>-country_code ).
          IF delete_granted = abap_false.
            APPEND VALUE #( %tky = travel-%tky
                            %msg = NEW /DMO/CM_FLIGHT_MESSAGES(
                                     textid   = /DMO/CM_FLIGHT_MESSAGES=>not_authorized_for_agencyid
                                     agency_id = travel-AgencyID
                                     severity = if_abap_behv_message=>severity-error )
                            %element-AgencyID = if_abap_behv=>mk-on
                           ) TO reported-travel.
          ENDIF.
        ENDIF.

        " operations on draft instances and on active instances that have no persistent before image (eg Update on newly created instance)
        " create authorization is checked, for newly created instances
      ELSE.
        update_granted = delete_granted = is_create_granted( ).
        IF update_granted = abap_false.
          APPEND VALUE #( %tky = travel-%tky
                          %msg = NEW /DMO/CM_FLIGHT_MESSAGES(
                                   textid   = /DMO/CM_FLIGHT_MESSAGES=>not_authorized
                                   severity = if_abap_behv_message=>severity-error )
                          %element-AgencyID = if_abap_behv=>mk-on
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( LET upd_auth = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                          del_auth = COND #( WHEN delete_granted = abap_true THEN if_abap_behv=>auth-allowed
                                             ELSE if_abap_behv=>auth-unauthorized )
                      IN
                       %tky = travel-%tky
                       %update                = upd_auth
                       %action-Edit           = upd_auth

                       %delete                = del_auth
                    ) TO result.
    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
```

#### Instance Authorization Beispiel

```abap
  METHOD get_instance_authorizations.
    data: update_requested type abap_bool,
          delete_requested type abap_bool.

    read entities of ZMIND2RAP_I_Travel in local mode
        ENTITY Travel
            Fields ( AgencyId )
            WITH CORRESPONDING #( keys )
        RESULT data(travels)
        failed failed.

    check travels is not INITIAL.

    update_requested = COND #( when requested_authorizations-%update = if_abap_behv=>mk-on then abap_true
*                              Diese Zeile nur wenn Draft Handling aktiviert ist
                               when requested_authorizations-%action-Edit = if_abap_behv=>mk-on then abap_true
                               else abap_false ).
    delete_requested = COND #( when requested_authorizations-%delete = if_abap_behv=>mk-on then abap_true
                               else abap_false ).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<agency>) GROUP BY <agency>-AgencyId.
        data(update_authorized) = cond #( when update_requested = abap_true
                                            then zcl_mind2rap_helper=>is_granted( agency = <agency>-AgencyId actvt = '02' )
                                          else abap_false ).

        data(delete_authorized) = cond #( when delete_requested = abap_true
                                            then zcl_mind2rap_helper=>is_granted( agency = <agency>-AgencyId actvt = '06' )
                                          else abap_false ).

        LOOP AT GROUP <agency> ASSIGNING FIELD-SYMBOL(<travel>).
            if ( update_requested = abap_true AND update_authorized = abap_false ) OR
               ( delete_requested = abap_true AND delete_authorized = abap_false ).
                append value #( %tky = <travel>-%tky
                                %msg = new_message(
                                    id = 'ZCM_MIND2RAP_TRAVEL'
                                    number = '007'
                                    severity = if_abap_behv_message=>severity-error
                                    v1 = <travel>-TravelId
                                ) ) to reported-travel.
            endif.

            append value #( let update_auth = cond #( when update_authorized = abap_true then if_abap_behv=>auth-allowed
                                                      else if_abap_behv=>auth-unauthorized )
                                delete_auth = cond #( when delete_authorized = abap_true then if_abap_behv=>auth-allowed
                                                      else if_abap_behv=>auth-unauthorized )
                            in
                                %tky = <travel>-%tky
                                %update = update_auth
                                %delete = delete_auth
*                               Diese Aktion nur wenn Draft Handling aktiviert ist
                                %action-Edit = update_auth ) to result.
        ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
```

## Sperren

### Pessimistische Sperren

```cds
define behavior for /DMO/I_Travel_D alias Travel
lock master
...
{
    ...
}

define behavior for /DMO/I_Booking_D alias Booking
lock dependent by _Travel
...
{
    ...
    association _Travel { }
}
```

### Optimistische Sperren

```cds
define behavior for /DMO/I_Travel_M alias Travel
etag master LocalLastChangedAt
...

define behavior for /DMO/I_Booking_D alias Booking
etag master LocalLastChangedAt
...
```

```cds
define behavior for ZI_SalesOrder alias SalesOrder
etag master LastChangedAt
...

define behavior for ZI_SalesOrderItem alias Item
etag dependant by _SalesOrder
...
{
    association _SalesOrder { }
}
```

## Draft Handling

```cds
managed;
strict;
with draft;
define behavior for /DMO/I_Travel_D alias Travel
```

### Draft Tabellen

```cds
...
define behavior for /DMO/I_Travel_D alias Travel
...
persistent table /dmo/a_travel_d
draft table /dmo/d_travel_d
...

define behavior for /DMO/I_Booking_D alias Booking
...
persistent table /dmo/a_booking_d
draft table /dmo/d_booking_d
...
```

### Draft etag Handling

```cds
define behavior for /DMO/I_Travel_D alias Travel
...
total etag LastChangedAt
...
```

### Draft Assoziationen

```cds
define behavior for /DMO/I_Travel_D alias Travel
...
{
    ...
    association _Booking { create; with draft; }
}
...
```

### Draft Aktionen

```cds
define behavior for /DMO/I_Travel_D alias Travel
...
{
    draft action Resume;
    draft action Edit;
    draft action Activate;
    draft action Discard;
    
    draft determine action Prepare
    {
        validation validateAgency;
        validation validateCustomer;
        ...
    }

    validation validateCustomer on save { ... }
    validation validateAgency on save { ... }
    ...
}
```

## Implementierung Speichersequenz

### Managed scenario with unmanaged save

```cds
define behavior for /DMO/I_BookSuppl_M alias booksuppl
implementation in class /DMO/BP_BOOKINGSUPPLEMENT_M unique
with unmanaged save
...
{
   ... 
}
```

```abap
CLASS lcl_save DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.

ENDCLASS.


CLASS lcl_save IMPLEMENTATION.

 DATA booksuppls_db TYPE STANDARD TABLE OF /dmo/booksuppl_m.

    " (1) Get instance data of all instances that have been created
    IF create-booksuppl IS NOT INITIAL.
      booksuppls_db = CORRESPONDING #( create-booksuppl ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_C' EXPORTING values = booksuppls_db .

    ENDIF.

    " (2) Get instance data of all instances that have been updated during the transaction
    booksuppls_db = CORRESPONDING #( update-booksuppl ).
    IF booksuppls_db IS NOT INITIAL.

      " Read all field values from database
      SELECT * FROM /dmo/booksuppl_m FOR ALL ENTRIES IN @booksuppls_db
               WHERE booking_supplement_id = @booksuppls_db-booking_supplement_id
               INTO TABLE @booksuppls_db .

      " Take over field values that have been changed during the transaction
      LOOP AT update-booksuppl ASSIGNING FIELD-SYMBOL(<unmanaged_booksuppl>).
        ASSIGN booksuppls_db[ travel_id  = <unmanaged_booksuppl>-travel_id
                              booking_id = <unmanaged_booksuppl>-booking_id
                   booking_supplement_id = <unmanaged_booksuppl>-booking_supplement_id
                            ] TO FIELD-SYMBOL(<booksuppl_db>).

        IF <unmanaged_booksuppl>-%control-supplement_id = if_abap_behv=>mk-on.
          <booksuppl_db>-supplement_id = <unmanaged_booksuppl>-supplement_id.
        ENDIF.

        IF <unmanaged_booksuppl>-%control-price = if_abap_behv=>mk-on.
          <booksuppl_db>-price = <unmanaged_booksuppl>-price.
        ENDIF.

        IF <unmanaged_booksuppl>-%control-currency_code = if_abap_behv=>mk-on.
          <booksuppl_db>-currency_code = <unmanaged_booksuppl>-currency_code.
        ENDIF.

      ENDLOOP.

      " Update the complete instance data
      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_U' EXPORTING values = booksuppls_db .

    ENDIF.

    " (3) Get keys of all travel instances that have been deleted during the transaction
    IF delete-booksuppl IS NOT INITIAL.
      booksuppls_db = CORRESPONDING #( delete-booksuppl ).

      CALL FUNCTION '/DMO/FLIGHT_BOOKSUPPL_D' EXPORTING values = booksuppls_db .

    ENDIF.


  ENDMETHOD.

ENDCLASS.
```
