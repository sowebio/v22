pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__testgui.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__testgui.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E067 : Short_Integer; pragma Import (Ada, E067, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "ada__exceptions_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__soft_links_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "system__exception_table_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__containers_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__io_exceptions_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "ada__strings_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "ada__strings__maps_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings__maps__constants_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ada__tags_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__streams_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "gnat_E");
   E036 : Short_Integer; pragma Import (Ada, E036, "interfaces__c_E");
   E018 : Short_Integer; pragma Import (Ada, E018, "system__exceptions_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__finalization_root_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "ada__finalization_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "system__storage_pools_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "system__checked_pools_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "gnat__debug_pools_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__object_reader_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "system__dwarf_lines_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__soft_links__initialize_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "system__traceback__symbolic_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "gnatcoll__memory_E");
   E404 : Short_Integer; pragma Import (Ada, E404, "ada__numerics_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__strings__utf_encoding_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__strings__text_output_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "interfaces__c__strings_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__file_control_block_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "system__file_io_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "ada__streams__stream_io_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "system__finalization_masters_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "system__storage_pools__subpools_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "ada__strings__unbounded_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "ada__strings__wide_wide_maps_E");
   E483 : Short_Integer; pragma Import (Ada, E483, "ada__strings__wide_wide_maps__wide_wide_constants_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "ada__strings__wide_wide_unbounded_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__task_info_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "system__task_primitives__operations_E");
   E522 : Short_Integer; pragma Import (Ada, E522, "system__regpat_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__calendar_E");
   E329 : Short_Integer; pragma Import (Ada, E329, "ada__calendar__delays_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "ada__calendar__time_zones_E");
   E331 : Short_Integer; pragma Import (Ada, E331, "ada__real_time_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "ada__text_io_E");
   E499 : Short_Integer; pragma Import (Ada, E499, "gnat__calendar_E");
   E502 : Short_Integer; pragma Import (Ada, E502, "gnat__calendar__time_io_E");
   E391 : Short_Integer; pragma Import (Ada, E391, "gnat__secure_hashes_E");
   E393 : Short_Integer; pragma Import (Ada, E393, "gnat__secure_hashes__sha1_E");
   E540 : Short_Integer; pragma Import (Ada, E540, "gnat__secure_hashes__sha2_common_E");
   E538 : Short_Integer; pragma Import (Ada, E538, "gnat__secure_hashes__sha2_64_E");
   E389 : Short_Integer; pragma Import (Ada, E389, "gnat__sha1_E");
   E536 : Short_Integer; pragma Import (Ada, E536, "gnat__sha512_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "system__assertions_E");
   E467 : Short_Integer; pragma Import (Ada, E467, "system__interrupt_management__operations_E");
   E256 : Short_Integer; pragma Import (Ada, E256, "system__pool_global_E");
   E519 : Short_Integer; pragma Import (Ada, E519, "gnat__expect_E");
   E337 : Short_Integer; pragma Import (Ada, E337, "gnat__sockets_E");
   E340 : Short_Integer; pragma Import (Ada, E340, "gnat__sockets__poll_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "gnat__sockets__thin_common_E");
   E342 : Short_Integer; pragma Import (Ada, E342, "gnat__sockets__thin_E");
   E429 : Short_Integer; pragma Import (Ada, E429, "system__pool_size_E");
   E325 : Short_Integer; pragma Import (Ada, E325, "system__regexp_E");
   E306 : Short_Integer; pragma Import (Ada, E306, "ada__directories_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "system__tasking__initialization_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "system__tasking__protected_objects_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "system__tasking__protected_objects__entries_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__tasking__queuing_E");
   E385 : Short_Integer; pragma Import (Ada, E385, "system__tasking__stages_E");
   E465 : Short_Integer; pragma Import (Ada, E465, "system__tasking__async_delays_E");
   E359 : Short_Integer; pragma Import (Ada, E359, "generic_unbounded_array_E");
   E441 : Short_Integer; pragma Import (Ada, E441, "generic_unbounded_ptr_array_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "object_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "object__handle_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "object__handle__generic_unbounded_array_E");
   E439 : Short_Integer; pragma Import (Ada, E439, "stack_storage_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "strings_edit_E");
   E401 : Short_Integer; pragma Import (Ada, E401, "strings_edit__base64_E");
   E417 : Short_Integer; pragma Import (Ada, E417, "strings_edit__fields_E");
   E243 : Short_Integer; pragma Import (Ada, E243, "strings_edit__integer_edit_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "gnat__sockets__server_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "gnat__sockets__connection_state_machine_E");
   E399 : Short_Integer; pragma Import (Ada, E399, "gnat__sockets__connection_state_machine__big_endian__unsigneds_E");
   E435 : Short_Integer; pragma Import (Ada, E435, "gnat__sockets__connection_state_machine__expected_sequence_E");
   E437 : Short_Integer; pragma Import (Ada, E437, "gnat__sockets__connection_state_machine__terminated_strings_E");
   E413 : Short_Integer; pragma Import (Ada, E413, "strings_edit__float_edit_E");
   E403 : Short_Integer; pragma Import (Ada, E403, "strings_edit__floats_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "strings_edit__floats_E");
   E415 : Short_Integer; pragma Import (Ada, E415, "strings_edit__quoted_E");
   E463 : Short_Integer; pragma Import (Ada, E463, "strings_edit__streams_E");
   E421 : Short_Integer; pragma Import (Ada, E421, "tables_E");
   E423 : Short_Integer; pragma Import (Ada, E423, "tables__names_E");
   E419 : Short_Integer; pragma Import (Ada, E419, "strings_edit__time_conversions_E");
   E387 : Short_Integer; pragma Import (Ada, E387, "gnat__sockets__connection_state_machine__http_server_E");
   E201 : Short_Integer; pragma Import (Ada, E201, "uxstrings_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "uxstrings__conversions_E");
   E453 : Short_Integer; pragma Import (Ada, E453, "uxstrings__hash_E");
   E245 : Short_Integer; pragma Import (Ada, E245, "uxstrings__text_io_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "gnoga_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "gnoga__application_E");
   E302 : Short_Integer; pragma Import (Ada, E302, "gnoga__server_E");
   E444 : Short_Integer; pragma Import (Ada, E444, "gnoga__server__mime_E");
   E450 : Short_Integer; pragma Import (Ada, E450, "gnoga__types_E");
   E448 : Short_Integer; pragma Import (Ada, E448, "gnoga__server__database_E");
   E530 : Short_Integer; pragma Import (Ada, E530, "gnoga__server__database__mysql_E");
   E532 : Short_Integer; pragma Import (Ada, E532, "gnoga__server__database__sqlite_E");
   E457 : Short_Integer; pragma Import (Ada, E457, "gnoga__server__model_E");
   E459 : Short_Integer; pragma Import (Ada, E459, "gnoga__server__model__queries_E");
   E446 : Short_Integer; pragma Import (Ada, E446, "gnoga__server__template_parser_E");
   E461 : Short_Integer; pragma Import (Ada, E461, "gnoga__server__template_parser__simple_E");
   E469 : Short_Integer; pragma Import (Ada, E469, "gnoga__gui__base_E");
   E327 : Short_Integer; pragma Import (Ada, E327, "gnoga__server__connection_E");
   E442 : Short_Integer; pragma Import (Ada, E442, "gnoga__server__connection__common_E");
   E474 : Short_Integer; pragma Import (Ada, E474, "gnoga__client__storage_E");
   E492 : Short_Integer; pragma Import (Ada, E492, "gnoga__gui__location_E");
   E480 : Short_Integer; pragma Import (Ada, E480, "gnoga__types__colors_E");
   E476 : Short_Integer; pragma Import (Ada, E476, "gnoga__gui__element_E");
   E486 : Short_Integer; pragma Import (Ada, E486, "gnoga__gui__document_E");
   E549 : Short_Integer; pragma Import (Ada, E549, "gnoga__gui__element__style_block_E");
   E482 : Short_Integer; pragma Import (Ada, E482, "gnoga__gui__view_E");
   E488 : Short_Integer; pragma Import (Ada, E488, "gnoga__gui__element__common_E");
   E542 : Short_Integer; pragma Import (Ada, E542, "gnoga__gui__element__form_E");
   E551 : Short_Integer; pragma Import (Ada, E551, "gnoga__gui__element__list_E");
   E544 : Short_Integer; pragma Import (Ada, E544, "gnoga__gui__element__table_E");
   E471 : Short_Integer; pragma Import (Ada, E471, "gnoga__gui__window_E");
   E300 : Short_Integer; pragma Import (Ada, E300, "gnoga__gui__navigator_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "gnoga__application__multi_connect_E");
   E547 : Short_Integer; pragma Import (Ada, E547, "gnoga__gui__plugin__jqueryui_E");
   E553 : Short_Integer; pragma Import (Ada, E553, "gnoga__gui__plugin__jqueryui__widget_E");
   E494 : Short_Integer; pragma Import (Ada, E494, "v22_E");
   E517 : Short_Integer; pragma Import (Ada, E517, "v22__uxs_E");
   E509 : Short_Integer; pragma Import (Ada, E509, "v22__fls_E");
   E497 : Short_Integer; pragma Import (Ada, E497, "v22__prg_E");
   E526 : Short_Integer; pragma Import (Ada, E526, "v22__sql_E");
   E511 : Short_Integer; pragma Import (Ada, E511, "v22__sys_E");
   E515 : Short_Integer; pragma Import (Ada, E515, "v22__tio_E");
   E506 : Short_Integer; pragma Import (Ada, E506, "v22__msg_E");
   E534 : Short_Integer; pragma Import (Ada, E534, "v22__gui_E");
   E561 : Short_Integer; pragma Import (Ada, E561, "v22__gui__breadcrumb_E");
   E555 : Short_Integer; pragma Import (Ada, E555, "v22__gui__crud_E");
   E557 : Short_Integer; pragma Import (Ada, E557, "v22__gui__footer_E");
   E563 : Short_Integer; pragma Import (Ada, E563, "v22__gui__user_menu_E");
   E559 : Short_Integer; pragma Import (Ada, E559, "v22__gui__header_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "v22__gui__finalize_body");
      begin
         E534 := E534 - 1;
         F1;
      end;
      E559 := E559 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "v22__gui__header__finalize_spec");
      begin
         F2;
      end;
      E563 := E563 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "v22__gui__user_menu__finalize_spec");
      begin
         F3;
      end;
      E557 := E557 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "v22__gui__footer__finalize_spec");
      begin
         F4;
      end;
      E555 := E555 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "v22__gui__crud__finalize_spec");
      begin
         F5;
      end;
      E561 := E561 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "v22__gui__breadcrumb__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "v22__gui__finalize_spec");
      begin
         F7;
      end;
      E526 := E526 - 1;
      E497 := E497 - 1;
      E509 := E509 - 1;
      E506 := E506 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "v22__msg__finalize_spec");
      begin
         F8;
      end;
      E494 := E494 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "v22__sql__finalize_spec");
      begin
         F9;
      end;
      declare
         procedure F10;
         pragma Import (Ada, F10, "v22__prg__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "v22__fls__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "v22__finalize_spec");
      begin
         F12;
      end;
      E553 := E553 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gnoga__gui__plugin__jqueryui__widget__finalize_spec");
      begin
         F13;
      end;
      E547 := E547 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gnoga__gui__plugin__jqueryui__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gnoga__application__multi_connect__finalize_body");
      begin
         E296 := E296 - 1;
         F15;
      end;
      E471 := E471 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gnoga__gui__window__finalize_spec");
      begin
         F16;
      end;
      E544 := E544 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "gnoga__gui__element__table__finalize_spec");
      begin
         F17;
      end;
      E551 := E551 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "gnoga__gui__element__list__finalize_spec");
      begin
         F18;
      end;
      E542 := E542 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "gnoga__gui__element__form__finalize_spec");
      begin
         F19;
      end;
      E482 := E482 - 1;
      E488 := E488 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gnoga__gui__element__common__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gnoga__gui__view__finalize_spec");
      begin
         F21;
      end;
      E549 := E549 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gnoga__gui__element__style_block__finalize_spec");
      begin
         F22;
      end;
      E486 := E486 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gnoga__gui__document__finalize_spec");
      begin
         F23;
      end;
      E476 := E476 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gnoga__gui__element__finalize_spec");
      begin
         F24;
      end;
      E492 := E492 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gnoga__gui__location__finalize_spec");
      begin
         F25;
      end;
      E474 := E474 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "gnoga__client__storage__finalize_spec");
      begin
         F26;
      end;
      declare
         procedure F27;
         pragma Import (Ada, F27, "gnoga__server__connection__finalize_body");
      begin
         E327 := E327 - 1;
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "gnoga__server__connection__common__finalize_spec");
      begin
         E442 := E442 - 1;
         F28;
      end;
      E469 := E469 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gnoga__gui__base__finalize_spec");
      begin
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gnoga__server__template_parser__finalize_body");
      begin
         E446 := E446 - 1;
         F30;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gnoga__server__template_parser__finalize_spec");
      begin
         F31;
      end;
      E459 := E459 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "gnoga__server__model__queries__finalize_spec");
      begin
         F32;
      end;
      E457 := E457 - 1;
      declare
         procedure F33;
         pragma Import (Ada, F33, "gnoga__server__model__finalize_spec");
      begin
         F33;
      end;
      E532 := E532 - 1;
      declare
         procedure F34;
         pragma Import (Ada, F34, "gnoga__server__database__sqlite__finalize_spec");
      begin
         F34;
      end;
      E530 := E530 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "gnoga__server__database__mysql__finalize_spec");
      begin
         F35;
      end;
      E448 := E448 - 1;
      declare
         procedure F36;
         pragma Import (Ada, F36, "gnoga__server__database__finalize_spec");
      begin
         F36;
      end;
      E450 := E450 - 1;
      declare
         procedure F37;
         pragma Import (Ada, F37, "gnoga__types__finalize_spec");
      begin
         F37;
      end;
      declare
         procedure F38;
         pragma Import (Ada, F38, "gnoga__server__finalize_body");
      begin
         E302 := E302 - 1;
         F38;
      end;
      declare
         procedure F39;
         pragma Import (Ada, F39, "gnoga__application__finalize_body");
      begin
         E294 := E294 - 1;
         F39;
      end;
      E116 := E116 - 1;
      declare
         procedure F40;
         pragma Import (Ada, F40, "gnoga__finalize_spec");
      begin
         F40;
      end;
      declare
         procedure F41;
         pragma Import (Ada, F41, "uxstrings__text_io__finalize_body");
      begin
         E245 := E245 - 1;
         F41;
      end;
      declare
         procedure F42;
         pragma Import (Ada, F42, "uxstrings__text_io__finalize_spec");
      begin
         F42;
      end;
      E201 := E201 - 1;
      declare
         procedure F43;
         pragma Import (Ada, F43, "uxstrings__finalize_spec");
      begin
         F43;
      end;
      E387 := E387 - 1;
      declare
         procedure F44;
         pragma Import (Ada, F44, "gnat__sockets__connection_state_machine__http_server__finalize_spec");
      begin
         F44;
      end;
      E419 := E419 - 1;
      declare
         procedure F45;
         pragma Import (Ada, F45, "strings_edit__time_conversions__finalize_spec");
      begin
         F45;
      end;
      E463 := E463 - 1;
      declare
         procedure F46;
         pragma Import (Ada, F46, "strings_edit__streams__finalize_spec");
      begin
         F46;
      end;
      E437 := E437 - 1;
      declare
         procedure F47;
         pragma Import (Ada, F47, "gnat__sockets__connection_state_machine__terminated_strings__finalize_spec");
      begin
         F47;
      end;
      E435 := E435 - 1;
      declare
         procedure F48;
         pragma Import (Ada, F48, "gnat__sockets__connection_state_machine__expected_sequence__finalize_spec");
      begin
         F48;
      end;
      E399 := E399 - 1;
      declare
         procedure F49;
         pragma Import (Ada, F49, "gnat__sockets__connection_state_machine__big_endian__unsigneds__finalize_spec");
      begin
         F49;
      end;
      declare
         procedure F50;
         pragma Import (Ada, F50, "gnat__sockets__connection_state_machine__finalize_body");
      begin
         E355 := E355 - 1;
         F50;
      end;
      declare
         procedure F51;
         pragma Import (Ada, F51, "gnat__sockets__connection_state_machine__finalize_spec");
      begin
         F51;
      end;
      E371 := E371 - 1;
      declare
         procedure F52;
         pragma Import (Ada, F52, "gnat__sockets__server__finalize_spec");
      begin
         F52;
      end;
      E401 := E401 - 1;
      declare
         procedure F53;
         pragma Import (Ada, F53, "strings_edit__base64__finalize_spec");
      begin
         F53;
      end;
      E439 := E439 - 1;
      declare
         procedure F54;
         pragma Import (Ada, F54, "stack_storage__finalize_spec");
      begin
         F54;
      end;
      E375 := E375 - 1;
      declare
         procedure F55;
         pragma Import (Ada, F55, "object__finalize_spec");
      begin
         F55;
      end;
      E195 := E195 - 1;
      declare
         procedure F56;
         pragma Import (Ada, F56, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F56;
      end;
      E306 := E306 - 1;
      declare
         procedure F57;
         pragma Import (Ada, F57, "ada__directories__finalize_spec");
      begin
         F57;
      end;
      E325 := E325 - 1;
      declare
         procedure F58;
         pragma Import (Ada, F58, "system__regexp__finalize_spec");
      begin
         F58;
      end;
      E429 := E429 - 1;
      declare
         procedure F59;
         pragma Import (Ada, F59, "system__pool_size__finalize_spec");
      begin
         F59;
      end;
      declare
         procedure F60;
         pragma Import (Ada, F60, "gnat__sockets__finalize_body");
      begin
         E337 := E337 - 1;
         F60;
      end;
      declare
         procedure F61;
         pragma Import (Ada, F61, "gnat__sockets__finalize_spec");
      begin
         F61;
      end;
      E519 := E519 - 1;
      declare
         procedure F62;
         pragma Import (Ada, F62, "gnat__expect__finalize_spec");
      begin
         F62;
      end;
      E256 := E256 - 1;
      declare
         procedure F63;
         pragma Import (Ada, F63, "system__pool_global__finalize_spec");
      begin
         F63;
      end;
      E133 := E133 - 1;
      declare
         procedure F64;
         pragma Import (Ada, F64, "ada__text_io__finalize_spec");
      begin
         F64;
      end;
      E231 := E231 - 1;
      declare
         procedure F65;
         pragma Import (Ada, F65, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         F65;
      end;
      E229 := E229 - 1;
      declare
         procedure F66;
         pragma Import (Ada, F66, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         F66;
      end;
      E316 := E316 - 1;
      declare
         procedure F67;
         pragma Import (Ada, F67, "ada__strings__unbounded__finalize_spec");
      begin
         F67;
      end;
      E351 := E351 - 1;
      declare
         procedure F68;
         pragma Import (Ada, F68, "system__storage_pools__subpools__finalize_spec");
      begin
         F68;
      end;
      E250 := E250 - 1;
      declare
         procedure F69;
         pragma Import (Ada, F69, "system__finalization_masters__finalize_spec");
      begin
         F69;
      end;
      E333 := E333 - 1;
      declare
         procedure F70;
         pragma Import (Ada, F70, "ada__streams__stream_io__finalize_spec");
      begin
         F70;
      end;
      declare
         procedure F71;
         pragma Import (Ada, F71, "system__file_io__finalize_body");
      begin
         E137 := E137 - 1;
         F71;
      end;
      declare
         procedure F72;
         pragma Import (Ada, F72, "gnatcoll__memory__finalize_body");
      begin
         E261 := E261 - 1;
         F72;
      end;
      declare
         procedure F73;
         pragma Import (Ada, F73, "gnat__debug_pools__finalize_body");
      begin
         E263 := E263 - 1;
         F73;
      end;
      declare
         procedure F74;
         pragma Import (Ada, F74, "gnat__debug_pools__finalize_spec");
      begin
         F74;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := '8';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, True, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, True, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, False, True, False, False, True, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, False, True, True, False, True, True, 
           False, True, True, True, True, False, True, False, 
           True, True, False, True, True, False, True, True, 
           False, True, False, False, False, True, False, True, 
           True, True, False, False, True, False, True, True, 
           True, False, True, True, False, True, True, True, 
           True, False, False, True, True, False, False, False, 
           False, True, True, True, True, True, True, False),
         Count => (0, 0, 0, 1, 2, 2, 3, 1, 5, 0),
         Unknown => (False, False, False, False, False, False, True, True, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      System.Scalar_Values.Initialize ('I', 'N');

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E008 := E008 + 1;
      Ada.Containers'Elab_Spec;
      E005 := E005 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E062 := E062 + 1;
      Ada.Strings'Elab_Spec;
      E048 := E048 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E050 := E050 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E054 := E054 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E099 := E099 + 1;
      Ada.Streams'Elab_Spec;
      E097 := E097 + 1;
      Gnat'Elab_Spec;
      E216 := E216 + 1;
      Interfaces.C'Elab_Spec;
      E036 := E036 + 1;
      System.Exceptions'Elab_Spec;
      E018 := E018 + 1;
      System.Finalization_Root'Elab_Spec;
      E105 := E105 + 1;
      Ada.Finalization'Elab_Spec;
      E095 := E095 + 1;
      System.Storage_Pools'Elab_Spec;
      E254 := E254 + 1;
      System.Checked_Pools'Elab_Spec;
      E279 := E279 + 1;
      Gnat.Debug_Pools'Elab_Spec;
      System.Object_Reader'Elab_Spec;
      E073 := E073 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E043 := E043 + 1;
      System.Os_Lib'Elab_Body;
      E067 := E067 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E089 := E089 + 1;
      E010 := E010 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E031 := E031 + 1;
      E014 := E014 + 1;
      Gnat.Debug_Pools'Elab_Body;
      E263 := E263 + 1;
      GNATCOLL.MEMORY'ELAB_BODY;
      E261 := E261 + 1;
      Ada.Numerics'Elab_Spec;
      E404 := E404 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E112 := E112 + 1;
      Ada.Strings.Text_Output'Elab_Spec;
      E110 := E110 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E349 := E349 + 1;
      System.File_Control_Block'Elab_Spec;
      E138 := E138 + 1;
      System.File_Io'Elab_Body;
      E137 := E137 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E333 := E333 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E250 := E250 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E351 := E351 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E316 := E316 + 1;
      Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      E229 := E229 + 1;
      Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants'Elab_Spec;
      E483 := E483 + 1;
      Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      E231 := E231 + 1;
      System.Task_Info'Elab_Spec;
      E165 := E165 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E159 := E159 + 1;
      System.Regpat'Elab_Spec;
      E522 := E522 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E118 := E118 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E329 := E329 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E124 := E124 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E331 := E331 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E133 := E133 + 1;
      Gnat.Calendar'Elab_Spec;
      E499 := E499 + 1;
      Gnat.Calendar.Time_Io'Elab_Spec;
      E502 := E502 + 1;
      E391 := E391 + 1;
      E393 := E393 + 1;
      E540 := E540 + 1;
      Gnat.Secure_Hashes.Sha2_64'Elab_Spec;
      E538 := E538 + 1;
      Gnat.Sha1'Elab_Spec;
      E389 := E389 + 1;
      Gnat.Sha512'Elab_Spec;
      E536 := E536 + 1;
      System.Assertions'Elab_Spec;
      E199 := E199 + 1;
      System.Interrupt_Management.Operations'Elab_Body;
      E467 := E467 + 1;
      System.Pool_Global'Elab_Spec;
      E256 := E256 + 1;
      Gnat.Expect'Elab_Spec;
      E519 := E519 + 1;
      Gnat.Sockets'Elab_Spec;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E347 := E347 + 1;
      E342 := E342 + 1;
      Gnat.Sockets'Elab_Body;
      E337 := E337 + 1;
      E340 := E340 + 1;
      System.Pool_Size'Elab_Spec;
      E429 := E429 + 1;
      System.Regexp'Elab_Spec;
      E325 := E325 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E306 := E306 + 1;
      System.Tasking.Initialization'Elab_Body;
      E183 := E183 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E193 := E193 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E195 := E195 + 1;
      System.Tasking.Queuing'Elab_Body;
      E191 := E191 + 1;
      System.Tasking.Stages'Elab_Body;
      E385 := E385 + 1;
      System.Tasking.Async_Delays'Elab_Body;
      E465 := E465 + 1;
      E359 := E359 + 1;
      E441 := E441 + 1;
      Object'Elab_Spec;
      Object'Elab_Body;
      E375 := E375 + 1;
      E373 := E373 + 1;
      E377 := E377 + 1;
      Stack_Storage'Elab_Spec;
      Stack_Storage'Elab_Body;
      E439 := E439 + 1;
      Strings_Edit'Elab_Spec;
      E241 := E241 + 1;
      Strings_Edit.Base64'Elab_Spec;
      Strings_Edit.Base64'Elab_Body;
      E401 := E401 + 1;
      E417 := E417 + 1;
      E243 := E243 + 1;
      GNAT.SOCKETS.SERVER'ELAB_SPEC;
      GNAT.SOCKETS.SERVER'ELAB_BODY;
      E371 := E371 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE'ELAB_BODY;
      E355 := E355 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.BIG_ENDIAN.UNSIGNEDS'ELAB_BODY;
      E399 := E399 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.EXPECTED_SEQUENCE'ELAB_BODY;
      E435 := E435 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.TERMINATED_STRINGS'ELAB_BODY;
      E437 := E437 + 1;
      E413 := E413 + 1;
      Strings_Edit.Floats'Elab_Body;
      E403 := E403 + 1;
      E415 := E415 + 1;
      Strings_Edit.Streams'Elab_Spec;
      Strings_Edit.Streams'Elab_Body;
      E463 := E463 + 1;
      E421 := E421 + 1;
      E423 := E423 + 1;
      Strings_Edit.Time_Conversions'Elab_Spec;
      Strings_Edit.Time_Conversions'Elab_Body;
      E419 := E419 + 1;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_SPEC;
      GNAT.SOCKETS.CONNECTION_STATE_MACHINE.HTTP_SERVER'ELAB_BODY;
      E387 := E387 + 1;
      Uxstrings'Elab_Spec;
      Uxstrings'Elab_Body;
      E201 := E201 + 1;
      E239 := E239 + 1;
      E453 := E453 + 1;
      Uxstrings.Text_Io'Elab_Spec;
      Uxstrings.Text_Io'Elab_Body;
      E245 := E245 + 1;
      Gnoga'Elab_Spec;
      Gnoga'Elab_Body;
      E116 := E116 + 1;
      Gnoga.Application'Elab_Body;
      E294 := E294 + 1;
      Gnoga.Server'Elab_Body;
      E302 := E302 + 1;
      E444 := E444 + 1;
      Gnoga.Types'Elab_Spec;
      E450 := E450 + 1;
      Gnoga.Server.Database'Elab_Spec;
      E448 := E448 + 1;
      Gnoga.Server.Database.Mysql'Elab_Spec;
      Gnoga.Server.Database.Mysql'Elab_Body;
      E530 := E530 + 1;
      Gnoga.Server.Database.Sqlite'Elab_Spec;
      Gnoga.Server.Database.Sqlite'Elab_Body;
      E532 := E532 + 1;
      Gnoga.Server.Model'Elab_Spec;
      Gnoga.Server.Model'Elab_Body;
      E457 := E457 + 1;
      Gnoga.Server.Model.Queries'Elab_Spec;
      E459 := E459 + 1;
      Gnoga.Server.Template_Parser'Elab_Spec;
      Gnoga.Server.Template_Parser'Elab_Body;
      E446 := E446 + 1;
      E461 := E461 + 1;
      Gnoga.Gui.Base'Elab_Spec;
      Gnoga.Server.Connection'Elab_Spec;
      Gnoga.Gui.Base'Elab_Body;
      E469 := E469 + 1;
      Gnoga.Server.Connection.Common'Elab_Spec;
      E442 := E442 + 1;
      Gnoga.Server.Connection'Elab_Body;
      E327 := E327 + 1;
      Gnoga.Client.Storage'Elab_Spec;
      Gnoga.Client.Storage'Elab_Body;
      E474 := E474 + 1;
      Gnoga.Gui.Location'Elab_Spec;
      Gnoga.Gui.Location'Elab_Body;
      E492 := E492 + 1;
      Gnoga.Types.Colors'Elab_Spec;
      E480 := E480 + 1;
      Gnoga.Gui.Element'Elab_Spec;
      Gnoga.Gui.Element'Elab_Body;
      E476 := E476 + 1;
      Gnoga.Gui.Document'Elab_Spec;
      Gnoga.Gui.Document'Elab_Body;
      E486 := E486 + 1;
      Gnoga.Gui.Element.Style_Block'Elab_Spec;
      Gnoga.Gui.Element.Style_Block'Elab_Body;
      E549 := E549 + 1;
      Gnoga.Gui.View'Elab_Spec;
      Gnoga.Gui.Element.Common'Elab_Spec;
      Gnoga.Gui.Element.Common'Elab_Body;
      E488 := E488 + 1;
      Gnoga.Gui.View'Elab_Body;
      E482 := E482 + 1;
      Gnoga.Gui.Element.Form'Elab_Spec;
      Gnoga.Gui.Element.Form'Elab_Body;
      E542 := E542 + 1;
      Gnoga.Gui.Element.List'Elab_Spec;
      Gnoga.Gui.Element.List'Elab_Body;
      E551 := E551 + 1;
      Gnoga.Gui.Element.Table'Elab_Spec;
      Gnoga.Gui.Element.Table'Elab_Body;
      E544 := E544 + 1;
      Gnoga.Gui.Window'Elab_Spec;
      Gnoga.Gui.Window'Elab_Body;
      E471 := E471 + 1;
      E300 := E300 + 1;
      Gnoga.Application.Multi_Connect'Elab_Body;
      E296 := E296 + 1;
      Gnoga.Gui.Plugin.Jqueryui'Elab_Spec;
      E547 := E547 + 1;
      Gnoga.Gui.Plugin.Jqueryui.Widget'Elab_Spec;
      Gnoga.Gui.Plugin.Jqueryui.Widget'Elab_Body;
      E553 := E553 + 1;
      v22'elab_spec;
      v22.fls'elab_spec;
      v22.prg'elab_spec;
      v22.sql'elab_spec;
      E494 := E494 + 1;
      v22.msg'elab_spec;
      E506 := E506 + 1;
      E509 := E509 + 1;
      E497 := E497 + 1;
      E526 := E526 + 1;
      E511 := E511 + 1;
      E515 := E515 + 1;
      E517 := E517 + 1;
      v22.gui'elab_spec;
      v22.gui.breadcrumb'elab_spec;
      v22.gui.breadcrumb'elab_body;
      E561 := E561 + 1;
      v22.gui.crud'elab_spec;
      v22.gui.crud'elab_body;
      E555 := E555 + 1;
      v22.gui.footer'elab_spec;
      v22.gui.footer'elab_body;
      E557 := E557 + 1;
      v22.gui.user_menu'elab_spec;
      v22.gui.user_menu'elab_body;
      E563 := E563 + 1;
      v22.gui.header'elab_spec;
      v22.gui.header'elab_body;
      E559 := E559 + 1;
      v22.gui'elab_body;
      E534 := E534 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_testgui");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/s-memory.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-msg.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-fls.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-prg.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-sql.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-sys.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-tio.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-uxs.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui-breadcrumb.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui-crud.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui-footer.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui-user_menu.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui-header.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-gui.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testgui.o
   --   -L/home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/
   --   -L/home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/
   --   -L/home/sr/opt/gnat-1124-23w/share/gpr/lib/gnatcoll/static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/gpr/production/static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/xmlada/xmlada_unicode.static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/xmlada/xmlada_sax.static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/xmlada/xmlada_input.static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/xmlada/xmlada_dom.static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/xmlada/xmlada_schema.static/
   --   -L/home/sr/opt/gnat-1124-23w/lib/gnoga/gnoga/
   --   -L/home/sr/opt/gnat-1124-23w/lib/components/lib_components/
   --   -L/home/sr/opt/gnat-1124-23w/lib/uxstrings/lib_uxstrings/
   --   -L/home/sr/opt/gnat-1124-23w/lib/gcc/x86_64-pc-linux-gnu/11.2.0/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lrt
   --   -lpthread
   --   -lm
   --   -ldl
--  END Object file/option list   

end ada_main;
