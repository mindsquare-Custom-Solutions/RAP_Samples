CLASS lcl_object_loader DEFINITION CREATE PRIVATE.

    PUBLIC SECTION.
      INTERFACES: lif_data_generator.
      TYPES: tt_object    TYPE STANDARD TABLE OF zro_object WITH KEY system_version tadir_object tadir_object_name object_type object_key,
             tt_successor TYPE STANDARD TABLE OF zro_obj_succesor WITH KEY system_version successor_uuid,
             BEGIN OF ts_successor,
               tadir_object   TYPE string,
               tadir_obj_name TYPE string,
               object_type    TYPE string,
               object_key     TYPE string,
             END OF ts_successor,
  
             BEGIN OF ts_object,
               tadir_object             TYPE string,
               tadir_obj_name           TYPE string,
               object_type              TYPE string,
               object_key               TYPE string,
               software_component       TYPE string,
               application_component    TYPE string,
               state                    TYPE string,
               successor_classification TYPE string,
               successors               TYPE STANDARD TABLE OF ts_successor WITH EMPTY KEY,
             END OF ts_object,
  
             BEGIN OF ts_api,
               format_version      TYPE string,
               object_release_info TYPE STANDARD TABLE OF ts_object WITH EMPTY KEY,
             END OF ts_api.
    PROTECTED SECTION.
    PRIVATE SECTION.
      CLASS-METHODS:
        load_data_url
          IMPORTING
            file_name     TYPE zro_file_name
          RETURNING
            VALUE(result) TYPE ts_api,
        convert_data
          IMPORTING
            json_data           TYPE ts_api
            system_version_uuid TYPE zro_sys_ver_id.
  
      CLASS-DATA: out        TYPE REF TO if_oo_adt_classrun_out,
                  objects    TYPE tt_object,
                  successors TYPE tt_successor.
  ENDCLASS.
  
  CLASS lcl_object_loader IMPLEMENTATION.
  
    METHOD lif_data_generator~create.
      IF out IS BOUND.
        lcl_object_loader=>out = out.
      ENDIF.
  
      IF out IS BOUND.
        out->write( '--> Delete Content.' ) ##NO_TEXT.
      ENDIF.
      DELETE FROM zro_obj_succesor.                       "#EC CI_NOWHERE
      DELETE FROM zro_object.                             "#EC CI_NOWHERE
  
      IF out IS BOUND.
        out->write( '--> Build Content.' ) ##NO_TEXT.
      ENDIF.
  
      SELECT * FROM zro_sys_ver INTO TABLE @DATA(system_versions). " UP TO 1 ROWS.
  
      LOOP AT system_versions ASSIGNING FIELD-SYMBOL(<system_version>).
        DATA(json_data) = load_data_url( <system_version>-file_name ).
        convert_data( json_data = json_data system_version_uuid = <system_version>-system_version ).
      ENDLOOP.
  
  
  
      IF out IS BOUND.
        out->write( '--> Insert Content.' ) ##NO_TEXT.
      ENDIF.
      INSERT zro_object FROM TABLE @objects.
      INSERT zro_obj_succesor FROM TABLE @successors.
  
      IF out IS BOUND.
        out->write( '--> Done.' ) ##NO_TEXT.
      ENDIF.
    ENDMETHOD.
  
    METHOD load_data_url.
      " Create HTTP client; send request
      TRY.
          DATA(lo_destination) = cl_http_destination_provider=>create_by_url( |{ zif_ro_const=>base_url }{ file_name }.json| ).
  
          DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
          DATA(lo_request) = lo_http_client->get_http_request( ).
  
          DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
          DATA(lv_json_results) = lo_response->get_text( ).
  
        CATCH cx_root INTO DATA(lx_exception).
          IF out IS BOUND.
            out->write( lx_exception->get_text( ) ).
          ENDIF.
      ENDTRY.
  
      TRY.
  
          xco_cp_json=>data->from_string( lv_json_results )->apply( VALUE #(
            ( xco_cp_json=>transformation->camel_case_to_underscore )
            ( xco_cp_json=>transformation->boolean_to_abap_bool )
          ) )->write_to( REF #( result ) ).
  
          " catch any error
        CATCH cx_root INTO DATA(lx_root).
          out->write( lx_root->get_text( ) ).
      ENDTRY.
    ENDMETHOD.
  
    METHOD convert_data.
      GET TIME STAMP FIELD DATA(timestamp).
      IF json_data-format_version = `1`.
        LOOP AT json_data-object_release_info ASSIGNING FIELD-SYMBOL(<object>).
          APPEND VALUE zro_object( system_version = system_version_uuid
                                   tadir_object = <object>-tadir_object
                                   tadir_object_name = <object>-tadir_obj_name
                                   object_type = <object>-object_type
                                   object_key = <object>-object_key
                                   software_component = <object>-software_component
                                   application_component = <object>-application_component
                                   state = SWITCH #( <object>-state
                                                     WHEN 'deprecated' THEN zif_ro_const=>state-deprecated
                                                     WHEN 'notToBeReleased' THEN zif_ro_const=>state-not_to_be_released
                                                     WHEN 'notToBeReleasedStable' THEN zif_ro_const=>state-not_to_be_released_stable
                                                     WHEN 'released' THEN zif_ro_const=>state-released )
                                   local_last_changed_on = timestamp ) TO objects.
  
          LOOP AT <object>-successors ASSIGNING FIELD-SYMBOL(<successor>).
            APPEND VALUE zro_obj_succesor( system_version = system_version_uuid
                                           successor_uuid = cl_system_uuid=>create_uuid_x16_static( )
                                           tadir_object = <object>-tadir_object
                                           tadir_object_name = <object>-tadir_obj_name
                                           object_type = <object>-object_type
                                           object_key = <object>-object_key
                                           successor_object = <successor>-tadir_object
                                           successor_object_name = <successor>-tadir_obj_name
                                           successor_object_type = <successor>-object_type
                                           successor_object_key = <successor>-object_key
                                           local_last_changed_on = timestamp ) TO successors.
          ENDLOOP.
        ENDLOOP.
      ELSE.
        IF out IS BOUND.
          out->write( | File format version { json_data-format_version } not supported! | ).
        ENDIF.
      ENDIF.
    ENDMETHOD.
  
  ENDCLASS.