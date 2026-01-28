CLASS zcl_ai_tool_weather_info DEFINITION
  PUBLIC
    INHERITING FROM zcl_ai_tool_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        name        TYPE string DEFAULT 'Weather_Info_Tool'
        description TYPE string DEFAULT 'Provides current weather conditions, specifically temperature in Celsius (°C) and wind speed in kilometers per hour (km/h), for the location defined by the given latitude and longitude.'.
  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.
    METHODS define_argument_metadata REDEFINITION.
  PRIVATE SECTION.
    TYPES BEGIN OF ts_weather_input.
    TYPES latitude TYPE string.
    TYPES longtitude TYPE string.
    TYPES END OF ts_weather_input.

    TYPES: BEGIN OF ts_weather_info,
             temperature TYPE string,
             windspeed   TYPE string,
             weathercode TYPE string,
           END OF ts_weather_info.

    TYPES: BEGIN OF ts_response,
             current_weather TYPE ts_weather_info,
           END OF ts_response.
ENDCLASS.



CLASS zcl_ai_tool_weather_info IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      name        = name
      description = description
    ).
  ENDMETHOD.

  METHOD do_execute.
    DATA weather_input TYPE ts_weather_input.
    /ui2/cl_json=>deserialize( EXPORTING json = input pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = weather_input ).
    DATA(latitude) = weather_input-latitude.
    DATA(longtitude) = weather_input-longtitude.

    TRY.
        IF latitude CP '*E*'.
          latitude = |{ CONV decfloat34( latitude ) }|.
        ENDIF.

        IF longtitude CP '*E*'.
          longtitude = |{ CONV decfloat34( longtitude ) }|.
        ENDIF.
      CATCH cx_root.
        " If conversion fails, proceed with original string or handle error
    ENDTRY.


    DATA(weather_api) = |https://api.open-meteo.com/v1/forecast?latitude={ latitude }&longitude={ longtitude }&current_weather=true|.

    TRY.
        " Create destination from URL
        DATA(http_dest) = cl_http_destination_provider=>create_by_url( weather_api ).

        " Create HTTP client
        DATA(http_client) = cl_web_http_client_manager=>create_by_http_destination( http_dest ).

        " Send GET request
        DATA(response) = http_client->execute( if_web_http_client=>get ).

        DATA(status) = response->get_status( ).
        IF status-code <> 200.
          DATA(error_message) = |HTTP request failed with status code { status-code }. \n Respond body: { response->get_text( ) } |.
          RAISE EXCEPTION TYPE zcx_ai_tool_error
            EXPORTING
              error_message = error_message.
          output = error_message.
          RETURN.
        ENDIF.
        DATA(response_text) = response->get_text( ).

        DATA response_struct TYPE ts_response.
        xco_cp_json=>data->from_string( response_text )->write_to( REF #( response_struct ) ).

        output = |The current temperature of location latitude: { latitude } and longtitude: { longtitude } is { response_struct-current_weather-temperature }°C with a wind speed of { response_struct-current_weather-windspeed } km/h.|.

      CATCH cx_root INTO DATA(err).
        output = |Got Error when calling the weather api. Error: { err->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD define_argument_metadata.
    DATA argument_latitude TYPE zcl_tool_schema=>ty_tool_argument.
    DATA argument_longtitude TYPE zcl_tool_schema=>ty_tool_argument.
    argument_latitude-name = 'latitude'.
    argument_latitude-type = 'number'.
    argument_latitude-description = 'The latitude of the target location. If the user provides a city name, convert it to coordinates automatically.'.
    argument_latitude-required = abap_true.
    argument_longtitude-name = 'longtitude'.
    argument_longtitude-type = 'number'.
    argument_longtitude-description = 'The longtitude of the target location. If the user provides a city name, convert it to coordinates automatically.'.
    argument_longtitude-required = abap_true.
    INSERT argument_latitude INTO TABLE arguments.
    INSERT argument_longtitude INTO TABLE arguments.
  ENDMETHOD.

ENDCLASS.
