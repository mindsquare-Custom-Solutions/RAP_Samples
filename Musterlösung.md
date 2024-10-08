 # Lösungsvorschläge

## 1 Datenmodell erstellen
Travel Root Entität (ohne Abhängigkeiten zu Booking und Booking Supplement):
```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Travel root view'
define root view entity ZR_<your-name-abbreviation>_TravelTP
  as select from ZMIND2E_I_Travel
//  composition [0..*] of ZR_<your-name-abbreviation>_BookingTP as _Booking
{
  key TravelID,
      AgencyID,
      CustomerID,
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
//      _Booking,
      _Currency,
      _Customer,
      _Status
}

```

Booking Entität (ohne Abhängigkeiten zu Travel und Booking Supplement):
```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking BO view'
define view entity ZR_<your-name-abbreviation>_BookingTP as select from ZMIND2E_I_Booking
//  association to parent ZR_<your-name-abbreviation>_TravelTP as _Travel on $projection.TravelID = _Travel.TravelID
//  composition [0..*] of ZR_<your-name-abbreviation>_BookingSupplementTP as _BookingSupplement
{
  key TravelID,
  key BookingID,
  BookingDate,
  CustomerID,
  CarrierId,
  BookingStatus,
  ConnectionID,
  FlightDate,
  FlightPrice,
  CurrencyCode,
  LocalLastChangedAt,
  /* Associations */
//  _BookingSupplement,
  _Carrier,
  _Connection,
  _Currency,
  _Customer,
  _Status,
  _Travel
}

```

Booking Supplements Entität (ohne Abhängigkeiten zu Travel und Booking):
```
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Supplement BO view'
define view entity ZR_<your-name-abbreviation>_BookingSupplementTP as select from ZMIND2E_I_BookingSupplement
//  association to parent ZR_<your-name-abbreviation>_BookingTP as _Booking on $projection.TravelID = _Booking.TravelID
//                                                    and $projection.BookingID = _Booking.BookingID 
{
  key TravelID,
  key BookingID,
  key BookingSupplementID,
  SupplementID,
  Price,
  CurrencyCode,
  
  /* Associations */
//  _Booking,
  _Currency,
  _Product,
  _SupplementText,
  _Travel
}

```

## 2 & 3 Behavior Definition anlegen
```
managed implementation in class zbp_i_<your-name-abbreviation>_travel unique;
strict ( 2 );

define behavior for ZR_<your-name-abbreviation>_TravelTP alias Travel
persistent table zmind2_travel
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;

  field ( readonly ) TravelId;

  mapping for zmind2_travel {
    TravelId = travel_id;
    AgencyId = agency_id;
    CustomerId = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    BookingFee = booking_fee;
    TotalPrice = total_price;
    CurrencyCode = currency_code;
    Description = description;
    Status = status;
    CreatedBy = createdby;
    CreatedAt = createdat;
    LastChangedBy = lastchangedby;
    LastChangedAt = lastchangedat;
    LocalLastChangedAt = locallastchangedat;
  }

  association _Booking { create; }
}

define behavior for ZR_<your-name-abbreviation>_BookingTP alias Booking
persistent table zmind2_booking
lock dependent by _Travel
authorization dependent by _Travel
//etag master <field_name>
{
  update;
  delete;

  field ( readonly ) TravelId, BookingId;

  mapping for zmind2_booking {
    TravelId = travel_id;
    BookingId = booking_id;
    BookingStatus = booking_status;
    BookingDate = booking_date;
    CustomerId = customer_id;
    CarrierId = carrier_id;
    ConnectionId = connection_id;
    FlightDate = flight_date;
    FlightPrice = flight_price;
    CurrencyCode = currency_code;
    LocalLastChangedAt = local_last_changed_at;
  }

  association _Travel;
  association _BookingSupplement { create; }
}

define behavior for ZR_<your-name-abbreviation>_BookingSupplementTP alias BookingSupplement
persistent table zmind2_book_supp
lock dependent by _Travel
authorization dependent by _Travel
//etag master <field_name>
{
  update;
  delete;

  field ( readonly ) BookingId, TravelId, BookingSupplementId;

  mapping for zmind2_book_supp {
    TravelId = travel_id;
    BookingId = booking_id;
    BookingSupplementId = booking_supplement_id;
    SupplementId = supplement_id;
    Price = price;
    CurrencyCode = currency_code;
    LastChangedAt = last_changed_at;
  }

  association _Travel;
  association _Booking;
}
```

## 4 Business Object Projections

Travel Entität:
```@EndUserText.label: 'Travel BO projection View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_<your-name-abbreviation>_TravelTP
  provider contract transactional_query as projection on ZR_<your-name-abbreviation>_TravelTP
{
  key TravelID,
  AgencyID,
  CustomerID,
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
  _Booking : redirected to composition child ZC_<your-name-abbreviation>_BookingTP,
  _Currency,
  _Customer,
  _Status
}
```

Booking Entität:
```@EndUserText.label: 'Booking BO projection view'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZC_<your-name-abbreviation>_BookingTP
  as projection on ZR_<your-name-abbreviation>_BookingTP
{
  key TravelID,
  key BookingID,
  BookingDate,
  CustomerID,
  CarrierId,
  BookingStatus,
  ConnectionID,
  FlightDate,
  FlightPrice,
  CurrencyCode,
  LocalLastChangedAt,
  
  /* Associations */
  _BookingSupplement : redirected to composition child ZC_<your-name-abbreviation>_BookingSupplementTP,
  _Carrier,
  _Connection,
  _Currency,
  _Customer,
  _Status,
  _Travel : redirected to parent ZC_<your-name-abbreviation>_TravelTP
}
```

Booking Supplement Entität:
```@EndUserText.label: 'Booking Supplement BO projection view'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view entity ZC_<your-name-abbreviation>_BookingSupplementTP
  as projection on ZR_<your-name-abbreviation>_BookingSupplementTP
{
  key TravelID,
  key BookingID,
  key BookingSupplementID,
      SupplementID,
      Price,
      CurrencyCode,

      /* Associations */
      _Booking : redirected to parent ZC_<your-name-abbreviation>_BookingTP,
      _Currency,
      _Product,
      _SupplementText,
      _Travel : redirected to ZC_<your-name-abbreviation>_TravelTP
}
```

Behavior Definition:
```projection;
strict ( 2 );

define behavior for ZC_<your-name-abbreviation>_TravelTP alias Travel
{
  use create;
  use update;
  use delete;

  use association _Booking { create; }
}

define behavior for ZC_<your-name-abbreviation>_BookingTP alias Booking
{
  use update;
  use delete;

  use association _Travel;
  use association _BookingSupplement { create; }
}

define behavior for ZC_<your-name-abbreviation>_BookingSupplementTP alias BookingSupplement
{
  use update;
  use delete;

  use association _Travel;
  use association _Booking;
}
```

## 3.2 Felder & Aktionen

Travel Entität:
```abap
create;
update;
delete;

mapping for zmind2_travel corresponding
{
    TravelId = travel_id;
    AgencyId = agency_id;
    CustomerId = customer_id;
    BeginDate = begin_date;
    EndDate = end_date;
    BookingFee = booking_fee;
    TotalPrice = total_price;
    CurrencyCode = currency_code;
}

association _Booking { create; }
```

Booking Entität:
```abap
update;
delete;

mapping for zmind2_booking corresponding
{
    TravelId = travel_id;
    BookingId = booking_id;
    BookingStatus = booking_status;
    BookingDate = booking_date;
    CustomerId = customer_id;
    CarrierId = carrier_id;
    ConnectionId = connection_id;
    FlightDate = flight_date;
    FlightPrice = flight_price;
    CurrencyCode = currency_code;
    LocalLastChangedAt = local_last_changed_at;
}

association _Travel;
```

## 3.3 EML

```abap
DATA: travels  TYPE TABLE FOR CREATE zmind2rap_i_TravelTP,
      bookings TYPE TABLE FOR CREATE ZMIND2RAP_I_TravelTP\_Booking.

travels = VALUE #( ( %cid = 'TRAVEL_1'
                        TravelId = 4000
                        Description = 'Travel 1 created by EML'
                        AgencyID = '70036'
                        CustomerId = '78'
                    )
                    ( %cid = 'TRAVEL_2'
                        TravelId = 4001
                        Description = 'Travel 2 created by EML'
                        AgencyID = '70029'
                        CustomerId = '33'
                    ) ).

bookings = VALUE #( ( %cid_ref = 'TRAVEL_1'
                        %target = VALUE #( ( %cid = 'BOOKING_1_1'
                                            BookingId = '1'
                                            BookingDate = '20230925'
                                            CarrierId = 'UA'
                                            ConnectionId = '1537'
                                            FlightDate = '20230610' )
                                            ( %cid = 'BOOKING_1_2'
                                            BookingId = '2'
                                            BookingDate = '20230925'
                                            CarrierId = 'LH'
                                            ConnectionId = '400'
                                            FlightDate = '20240406' ) ) )
                    ( %cid_ref = 'TRAVEL_2'
                        %target = VALUE #( ( %cid = 'BOOKING_2_1'
                                            BookingId = '1'
                                            BookingDate = '20230925'
                                            CarrierId = 'AA'
                                            ConnectionId = '18'
                                            FlightDate = '20240404' ) ) ) ).

MODIFY ENTITIES OF zmind2rap_i_travel
    ENTITY Travel
        CREATE SET FIELDS WITH travels
        CREATE BY \_Booking SET FIELDS WITH bookings
    MAPPED DATA(mapped)
    FAILED DATA(failed)
    REPORTED DATA(reported).
```

## 3.4 Feature Control

Semantische Annotationen Travel:
```cds
@Semantics.user.createdBy: true
createdby                                                     as CreatedBy,

@Semantics.systemDateTime.createdAt: true
createdat                                                     as CreatedAt,

@Semantics.user.lastChangedBy: true
lastchangedby                                                 as LastChangedBy,

@Semantics.systemDateTime.lastChangedAt: true
lastchangedat                                                 as LastChangedAt,

@Semantics.systemDateTime.localInstanceLastChangedAt: true
locallastchangedat                                            as LocalLastChangedAt,
```

Semantische Annotationen Booking:
```cds
@Semantics.systemDateTime.localInstanceLastChangedAt: true
local_last_changed_at as LocalLastChangedAt,
```

Behavior Definition Travel:
```abap
field ( readonly ) Status, CreatedAt, CreatedBy, LastChangedBy, LastChangedAt, LocalLastChangedAt, TotalPrice, CustomerName, StatusCriticality;
field ( mandatory ) AgencyId, CustomerId, CurrencyCode;
field ( mandatory : create, readonly : update ) TravelID;
```

Behavior Definition Booking:
```abap
field ( readonly ) TravelId, LocalLastChangedAt, BookingStatus;
field ( mandatory ) CarrierId, ConnectionId, FlightDate;
field ( readonly : update ) BookingDate;
field ( mandatory : create, readonly : update ) BookingID, CurrencyCode, FlightPrice;
```

## 3.5 Nummernvergabe

Nummernvergabe Travel:
```abap
METHOD earlynumbering_create.
    DATA: entity        TYPE STRUCTURE FOR CREATE ZMIND2RAP_I_Travel,
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
            object            = 'ZMS_TRVL'
            quantity          = CONV #( lines( entities_wo_travelid ) )
          IMPORTING
            number            = DATA(number_range_key)
            returncode        = DATA(numer_range_return_code)
            returned_quantity = DATA(number_range_returned_quantity) ).
      CATCH cx_number_ranges INTO DATA(lx_number_ranges).
        LOOP AT entities_wo_travelid INTO entity.
          APPEND VALUE #( %cid = entity-%cid
                          %key = entity-%key
                          %is_draft = entity-%is_draft
                          %msg = lx_number_ranges
                        ) TO reported-travel.
          APPEND VALUE #( %cid = entity-%cid
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

      APPEND VALUE #( %cid = entity-%cid
                      %key = entity-%key
                      %is_draft = entity-%is_draft
                    ) TO mapped-travel.
    ENDLOOP.
ENDMETHOD.
```

Nummernvergabe Booking:
```abap
METHOD earlynumbering_cba_Booking.
    DATA: max_booking_id TYPE zmind2_booking_id.

    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Travel BY \_Booking
            FROM CORRESPONDING #( entities )
            LINK DATA(bookings).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel_group>) GROUP BY <travel_group>-TravelId.
      max_booking_id = REDUCE #( INIT max = CONV zmind2_booking_id( '0' )
                                 FOR booking IN bookings USING KEY entity WHERE ( source-TravelId = <travel_group>-TravelId )
                                 NEXT max = COND zmind2_booking_id( WHEN booking-target-BookingId > max THEN booking-target-BookingId
                                                                    ELSE max ) ).

      LOOP AT entities ASSIGNING FIELD-SYMBOL(<travel>) USING KEY entity WHERE TravelId = <travel_group>-TravelId.
        LOOP AT <travel>-%target ASSIGNING FIELD-SYMBOL(<booking_wo_numbers>).
          APPEND CORRESPONDING #( <booking_wo_numbers> ) TO mapped-booking ASSIGNING FIELD-SYMBOL(<mapped_booking>).
          IF <booking_wo_numbers>-BookingId IS INITIAL.
            max_booking_id += 1.
            <mapped_booking>-BookingId = max_booking_id.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
ENDMETHOD.
```

Behavior Definition Travel:
```abap
field ( readonly ) TravelId, Status, CreatedAt, CreatedBy, LastChangedBy, LastChangedAt, LocalLastChangedAt, TotalPrice, CustomerName, StatusCriticality;
field ( mandatory ) AgencyId, CustomerId;
field ( mandatory : create, readonly : update ) CurrencyCode;
```

Behavior Definition Booking:
```abap
field ( readonly ) TravelId, LocalLastChangedAt, BookingId, BookingStatus;
field ( mandatory ) CarrierId, ConnectionId, FlightDate;
field ( readonly : update ) BookingDate;
field ( mandatory : create, readonly : update ) CurrencyCode, FlightPrice;
```

## 3.6 Ermittlungen

Travel Behavior Definition:
```abap
determination setStatusOnCreate on modify { create; }
```

Travel Implentierung:
```abap
METHOD setStatusOnCreate.
    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Travel
            FIELDS ( Status )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    DELETE travels WHERE Status IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    MODIFY ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Travel
            UPDATE FIELDS ( Status )
            WITH VALUE #( FOR travel IN travels
                          ( %tky = travel-%tky
                            Status = zif_mind2_travel=>travel-status-new ) ).
ENDMETHOD.
```

Booking Behavior Definition:
```abap
determination setBookingDate on modify { create; }
determination setBookingStatus on modify { create; }
```

Booking Implementierung:
```abap
METHOD setBookingStatus.
    DATA status TYPE zmind2_travel_status.

    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
       ENTITY Booking BY \_Travel
           FIELDS ( Status )
           WITH CORRESPONDING #( keys )
       RESULT DATA(travels).

    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Booking
            FIELDS ( BookingStatus )
            WITH CORRESPONDING #( keys )
        RESULT DATA(bookings).

    DELETE bookings WHERE BookingStatus IS NOT INITIAL.
    CHECK bookings IS NOT INITIAL.

    LOOP AT bookings INTO DATA(booking).
      IF line_exists( travels[ KEY draft TravelId = booking-%key-TravelId %is_draft = booking-%is_draft ] ).
        status = travels[ KEY draft TravelId = booking-%key-TravelId %is_draft = booking-%is_draft ]-Status.
      ENDIF.

      IF status IS INITIAL.
        status = zif_mind2_travel=>booking-status-new.
      ENDIF.

      MODIFY ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Booking
          UPDATE FIELDS ( BookingStatus )
          WITH VALUE #( ( %tky = booking-%tky
                          BookingStatus = status ) ).

      CLEAR status.
    ENDLOOP.
ENDMETHOD.

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

## 3.7 Validierung

Travel Behavior Definition:
```abap
validation validateDates on save { field BeginDate, EndDate; }
validation validateCustomer on save { field CustomerId; }
```

Travel Date Implementierung:
```abap
  METHOD validateDates.
    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Travel
            FIELDS ( BeginDate EndDate )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      IF travel-EndDate < travel-BeginDate.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'validateDates'
                        %msg = new_message(
                            id = 'ZCM_MIND2RAP_TRAVEL'
                            number = '001'
                            severity = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate = if_abap_behv=>mk-on
        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
ENDMETHOD.
```

Travel Customer:
```abap
  METHOD validateCustomer.
    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Travel
            FIELDS ( CustomerId )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF ZMIND2E_I_Customer WITH UNIQUE KEY CustomerId.

    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING CustomerId = CustomerId EXCEPT * ).
    DELETE customers WHERE CustomerId IS INITIAL.
    IF customers IS NOT INITIAL.
      SELECT
          FROM ZMIND2E_I_Customer
          FIELDS CustomerId
          FOR ALL ENTRIES IN @customers
          WHERE CustomerId = @customers-CustomerId
          INTO TABLE @DATA(customers_db).
    ENDIF.

    LOOP AT travels INTO DATA(travel).
      IF travel-CustomerId IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'validateCustomer'
                        %msg = new_message(
                            id = 'ZCM_MIND2RAP_TRAVEL'
                            number = '009'
                            severity = if_abap_behv_message=>severity-error
                            v1 = travel-CustomerId )
                        %element-CustomerId = if_abap_behv=>mk-on
                        ) TO reported-travel.
      ELSEIF NOT line_exists( customers_db[ CustomerId = travel-CustomerId ] ) .
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'validateCustomer'
                        %msg = new_message(
                            id = 'ZCM_MIND2RAP_TRAVEL'
                            number = '002'
                            severity = if_abap_behv_message=>severity-error
                            v1 = travel-CustomerId )
                        %element-CustomerId = if_abap_behv=>mk-on
                        ) TO reported-travel.
      ENDIF.
    ENDLOOP.
ENDMETHOD.
```

## 3.8 Aktionen

Travel Behavior Definition:
```abap
action changeCurrency parameter ZMIND2RAP_A_BookChangeCurr result [1] $self;
```

Travel Implementation:
```abap
METHOD changeCurrency.
    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
        ENTITY Booking
            FIELDS ( BookingDate FlightPrice CurrencyCode )
            WITH CORRESPONDING #( keys )
        RESULT DATA(bookings)
        FAILED failed.

    LOOP AT bookings ASSIGNING FIELD-SYMBOL(<booking>).
      DATA(target_currency) = keys[ KEY draft %tky = <booking>-%tky ]-%param-Currency.

      IF NOT zcl_mind2rap_helper=>validate_currency( target_currency ).
        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
        APPEND VALUE #( %tky = <booking>-%tky
                        %action-changecurrency = if_abap_behv=>mk-on
                        %msg = new_message(
                            id = 'ZCM_MIND2RAP_TRAVEL'
                            number = '003'
                            severity = if_abap_behv_message=>severity-error
                            v1 = target_currency )
                        ) TO reported-booking.
        CONTINUE.
      ENDIF.

      zcl_mind2rap_amdp=>convert_currency( EXPORTING iv_amount               = <booking>-FlightPrice
                                                     iv_currency_code_source = <booking>-CurrencyCode
                                                     iv_exchange_rate_date   = <booking>-BookingDate
                                                     iv_currency_code_target = target_currency
                                           IMPORTING ev_amount               = DATA(amount_in_target_currency) ).

      IF amount_in_target_currency IS INITIAL.
        APPEND VALUE #( %tky = <booking>-%tky ) TO failed-booking.
        APPEND VALUE #( %tky = <booking>-%tky
                        %msg = new_message(
                            id = 'ZCM_MIND2RAP_TRAVEL'
                            number = '004'
                            severity = if_abap_behv_message=>severity-error
                            v1 = <booking>-CurrencyCode
                            v2 = target_currency )
                        ) TO reported-booking.
        CONTINUE.
      ENDIF.

      MODIFY ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
           ENTITY Booking
              UPDATE FIELDS ( FlightPrice CurrencyCode )
              WITH VALUE #( ( %tky = <booking>-%tky
                              CurrencyCode = target_currency
                              FlightPrice = amount_in_target_currency ) ).
    ENDLOOP.

    READ ENTITIES OF ZMIND2RAP_I_Travel IN LOCAL MODE
      ENTITY Booking
         ALL FIELDS WITH
         CORRESPONDING #( keys )
       RESULT bookings.

    result = VALUE #( FOR booking IN bookings
                      ( %tky = booking-%tky
                        %param = booking ) ).
ENDMETHOD.
```

## 3.9 Sperren

Travel Behavior Definition:
```abap
define behavior for ZMIND2RAP_I_Travel alias Travel
persistent table zmind2_travel
lock master
authorization master ( instance )
early numbering
etag master LocalLastChangedAt
```

Booking Behavior Definition:
```abap
define behavior for ZMIND2RAP_I_Booking alias Booking
persistent table zmind2_booking
lock dependent by _Travel
authorization dependent by _Travel
early numbering
etag master LocalLastChangedAt
```

## 3.10

Behavior Definition Travel:
```abap
define behavior for ZMIND2RAP_I_Travel alias Travel
persistent table zmind2_travel
draft table zmind2_d_travel
lock master total etag LastChangedAt
authorization master ( instance )
early numbering
etag master LocalLastChangedAt
{
    ...
    draft action Resume;
    draft action Edit;
    draft action Activate;
    draft action Discard;

    draft determine action Prepare
    {
        validation validateDates;
        validation validateCustomer;
    }

    association _Booking { create; with draft; }
}
```

Behavior Definition Booking:
```abap
define behavior for ZMIND2RAP_I_Booking alias Booking
persistent table zmind2_booking
draft table zmind2_d_booking
lock dependent by _Travel
authorization dependent by _Travel
early numbering
etag master LocalLastChangedAt
{
    ...
    association _Travel { with draft; }
}
```

Behavior Projection:
```abap
projection;
strict;
use draft;

define behavior for ZMIND2RAP_C_Travel alias Travel
use etag
{
  ...
  use action Edit;
  use action Activate;
  use action Discard;
  use action Prepare;
  use action Resume;

  use association _Booking { create; with draft; }
}

define behavior for ZMIND2RAP_C_Booking alias Booking
use etag
{
  ...

  use association _Travel{ with draft; }
}
```
