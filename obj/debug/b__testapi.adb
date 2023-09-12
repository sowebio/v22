pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__testapi.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__testapi.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E073 : Short_Integer; pragma Import (Ada, E073, "system__os_lib_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__exceptions_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__strings__maps_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__strings__maps__constants_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__tags_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "ada__streams_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "gnat_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "interfaces__c_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "system__finalization_root_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "ada__finalization_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__storage_pools_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "system__checked_pools_E");
   E180 : Short_Integer; pragma Import (Ada, E180, "gnat__debug_pools_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__object_reader_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "system__dwarf_lines_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__soft_links__initialize_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "system__traceback__symbolic_E");
   E178 : Short_Integer; pragma Import (Ada, E178, "gnatcoll__memory_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "ada__strings__utf_encoding_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "ada__strings__text_output_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "interfaces__c__strings_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__file_control_block_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "system__file_io_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "system__finalization_masters_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "system__storage_pools__subpools_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__strings__unbounded_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "ada__strings__wide_wide_maps_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "ada__strings__wide_wide_unbounded_E");
   E317 : Short_Integer; pragma Import (Ada, E317, "system__task_info_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "system__task_primitives__operations_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "system__regpat_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__calendar_E");
   E265 : Short_Integer; pragma Import (Ada, E265, "ada__calendar__delays_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ada__calendar__time_zones_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "ada__text_io_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "gnat__calendar_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "gnat__calendar__time_io_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "gnat__directory_operations_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "system__assertions_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__pool_global_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "gnat__expect_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "system__regexp_E");
   E241 : Short_Integer; pragma Import (Ada, E241, "ada__directories_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "gnat__command_line_E");
   E333 : Short_Integer; pragma Import (Ada, E333, "system__tasking__initialization_E");
   E343 : Short_Integer; pragma Import (Ada, E343, "system__tasking__protected_objects_E");
   E345 : Short_Integer; pragma Import (Ada, E345, "system__tasking__protected_objects__entries_E");
   E341 : Short_Integer; pragma Import (Ada, E341, "system__tasking__queuing_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "strings_edit_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "strings_edit__integer_edit_E");
   E209 : Short_Integer; pragma Import (Ada, E209, "uxstrings_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "uxstrings__conversions_E");
   E376 : Short_Integer; pragma Import (Ada, E376, "uxstrings__hash_E");
   E355 : Short_Integer; pragma Import (Ada, E355, "uxstrings__text_io_E");
   E303 : Short_Integer; pragma Import (Ada, E303, "gnoga_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "gnoga__server_E");
   E370 : Short_Integer; pragma Import (Ada, E370, "gnoga__types_E");
   E368 : Short_Integer; pragma Import (Ada, E368, "gnoga__server__database_E");
   E380 : Short_Integer; pragma Import (Ada, E380, "gnoga__server__database__mysql_E");
   E382 : Short_Integer; pragma Import (Ada, E382, "gnoga__server__database__sqlite_E");
   E239 : Short_Integer; pragma Import (Ada, E239, "v22_E");
   E280 : Short_Integer; pragma Import (Ada, E280, "v22__uxs_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "v22__fls_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "v22__prg_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "v22__sql_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "v22__sys_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "v22__tio_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "v22__msg_E");
   E394 : Short_Integer; pragma Import (Ada, E394, "testapi_msg_E");
   E396 : Short_Integer; pragma Import (Ada, E396, "testapi_sql_E");
   E398 : Short_Integer; pragma Import (Ada, E398, "testapi_sys_E");
   E400 : Short_Integer; pragma Import (Ada, E400, "testapi_tio_E");
   E386 : Short_Integer; pragma Import (Ada, E386, "v22__cfg_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "testapi_cfg_E");
   E390 : Short_Integer; pragma Import (Ada, E390, "v22__crl_E");
   E392 : Short_Integer; pragma Import (Ada, E392, "v22__crl__callbacks_E");
   E388 : Short_Integer; pragma Import (Ada, E388, "testapi_crl_E");
   E402 : Short_Integer; pragma Import (Ada, E402, "v22__net_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E402 := E402 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "v22__net__finalize_spec");
      begin
         F1;
      end;
      E386 := E386 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "v22__cfg__finalize_spec");
      begin
         F2;
      end;
      E295 := E295 - 1;
      E250 := E250 - 1;
      E255 := E255 - 1;
      E252 := E252 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "v22__msg__finalize_spec");
      begin
         F3;
      end;
      E239 := E239 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "v22__sql__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "v22__prg__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "v22__fls__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "v22__finalize_spec");
      begin
         F7;
      end;
      E382 := E382 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gnoga__server__database__sqlite__finalize_spec");
      begin
         F8;
      end;
      E380 := E380 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gnoga__server__database__mysql__finalize_spec");
      begin
         F9;
      end;
      E368 := E368 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gnoga__server__database__finalize_spec");
      begin
         F10;
      end;
      E370 := E370 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gnoga__types__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gnoga__server__finalize_body");
      begin
         E366 := E366 - 1;
         F12;
      end;
      E303 := E303 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gnoga__finalize_spec");
      begin
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "uxstrings__text_io__finalize_body");
      begin
         E355 := E355 - 1;
         F14;
      end;
      declare
         procedure F15;
         pragma Import (Ada, F15, "uxstrings__text_io__finalize_spec");
      begin
         F15;
      end;
      E209 := E209 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "uxstrings__finalize_spec");
      begin
         F16;
      end;
      E345 := E345 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F17;
      end;
      E241 := E241 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ada__directories__finalize_spec");
      begin
         F18;
      end;
      E204 := E204 - 1;
      declare
         procedure F19;
         pragma Import (Ada, F19, "system__regexp__finalize_spec");
      begin
         F19;
      end;
      E288 := E288 - 1;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gnat__expect__finalize_spec");
      begin
         F20;
      end;
      E173 := E173 - 1;
      declare
         procedure F21;
         pragma Import (Ada, F21, "system__pool_global__finalize_spec");
      begin
         F21;
      end;
      E149 := E149 - 1;
      declare
         procedure F22;
         pragma Import (Ada, F22, "ada__text_io__finalize_spec");
      begin
         F22;
      end;
      E233 := E233 - 1;
      declare
         procedure F23;
         pragma Import (Ada, F23, "ada__strings__wide_wide_unbounded__finalize_spec");
      begin
         F23;
      end;
      E231 := E231 - 1;
      declare
         procedure F24;
         pragma Import (Ada, F24, "ada__strings__wide_wide_maps__finalize_spec");
      begin
         F24;
      end;
      E118 := E118 - 1;
      declare
         procedure F25;
         pragma Import (Ada, F25, "ada__strings__unbounded__finalize_spec");
      begin
         F25;
      end;
      E197 := E197 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "system__storage_pools__subpools__finalize_spec");
      begin
         F26;
      end;
      E167 := E167 - 1;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__finalization_masters__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "system__file_io__finalize_body");
      begin
         E153 := E153 - 1;
         F28;
      end;
      declare
         procedure F29;
         pragma Import (Ada, F29, "gnatcoll__memory__finalize_body");
      begin
         E178 := E178 - 1;
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "gnat__debug_pools__finalize_body");
      begin
         E180 := E180 - 1;
         F30;
      end;
      declare
         procedure F31;
         pragma Import (Ada, F31, "gnat__debug_pools__finalize_spec");
      begin
         F31;
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
          (False, False, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, False, True, False, False, True, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, False, True, True, False, False, True, 
           False, True, False, True, True, False, True, False, 
           True, False, False, False, True, False, True, False, 
           False, False, False, False, False, False, False, True, 
           True, True, False, False, True, False, True, True, 
           True, False, True, True, False, True, True, True, 
           True, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False),
         Count => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Unknown => (False, False, False, False, False, False, False, False, False, False));
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
      E025 := E025 + 1;
      Ada.Containers'Elab_Spec;
      E039 := E039 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E054 := E054 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E056 := E056 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E060 := E060 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E097 := E097 + 1;
      Ada.Streams'Elab_Spec;
      E136 := E136 + 1;
      Gnat'Elab_Spec;
      E102 := E102 + 1;
      Interfaces.C'Elab_Spec;
      E044 := E044 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Finalization_Root'Elab_Spec;
      E138 := E138 + 1;
      Ada.Finalization'Elab_Spec;
      E134 := E134 + 1;
      System.Storage_Pools'Elab_Spec;
      E171 := E171 + 1;
      System.Checked_Pools'Elab_Spec;
      E195 := E195 + 1;
      Gnat.Debug_Pools'Elab_Spec;
      System.Object_Reader'Elab_Spec;
      E079 := E079 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E049 := E049 + 1;
      System.Os_Lib'Elab_Body;
      E073 := E073 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E021 := E021 + 1;
      E013 := E013 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E038 := E038 + 1;
      E008 := E008 + 1;
      Gnat.Debug_Pools'Elab_Body;
      E180 := E180 + 1;
      GNATCOLL.MEMORY'ELAB_BODY;
      E178 := E178 + 1;
      Ada.Strings.Utf_Encoding'Elab_Spec;
      E123 := E123 + 1;
      Ada.Strings.Text_Output'Elab_Spec;
      E121 := E121 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E261 := E261 + 1;
      System.File_Control_Block'Elab_Spec;
      E154 := E154 + 1;
      System.File_Io'Elab_Body;
      E153 := E153 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E167 := E167 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E197 := E197 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E118 := E118 + 1;
      Ada.Strings.Wide_Wide_Maps'Elab_Spec;
      E231 := E231 + 1;
      Ada.Strings.Wide_Wide_Unbounded'Elab_Spec;
      E233 := E233 + 1;
      System.Task_Info'Elab_Spec;
      E317 := E317 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E311 := E311 + 1;
      System.Regpat'Elab_Spec;
      E291 := E291 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E006 := E006 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E265 := E265 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E109 := E109 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E149 := E149 + 1;
      Gnat.Calendar'Elab_Spec;
      E104 := E104 + 1;
      Gnat.Calendar.Time_Io'Elab_Spec;
      E116 := E116 + 1;
      Gnat.Directory_Operations'Elab_Spec;
      Gnat.Directory_Operations'Elab_Body;
      E162 := E162 + 1;
      System.Assertions'Elab_Spec;
      E226 := E226 + 1;
      System.Pool_Global'Elab_Spec;
      E173 := E173 + 1;
      Gnat.Expect'Elab_Spec;
      E288 := E288 + 1;
      System.Regexp'Elab_Spec;
      E204 := E204 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E241 := E241 + 1;
      Gnat.Command_Line'Elab_Spec;
      Gnat.Command_Line'Elab_Body;
      E160 := E160 + 1;
      System.Tasking.Initialization'Elab_Body;
      E333 := E333 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E343 := E343 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E345 := E345 + 1;
      System.Tasking.Queuing'Elab_Body;
      E341 := E341 + 1;
      Strings_Edit'Elab_Spec;
      E351 := E351 + 1;
      E353 := E353 + 1;
      Uxstrings'Elab_Spec;
      Uxstrings'Elab_Body;
      E209 := E209 + 1;
      E349 := E349 + 1;
      E376 := E376 + 1;
      Uxstrings.Text_Io'Elab_Spec;
      Uxstrings.Text_Io'Elab_Body;
      E355 := E355 + 1;
      Gnoga'Elab_Spec;
      Gnoga'Elab_Body;
      E303 := E303 + 1;
      Gnoga.Server'Elab_Body;
      E366 := E366 + 1;
      Gnoga.Types'Elab_Spec;
      E370 := E370 + 1;
      Gnoga.Server.Database'Elab_Spec;
      E368 := E368 + 1;
      Gnoga.Server.Database.Mysql'Elab_Spec;
      Gnoga.Server.Database.Mysql'Elab_Body;
      E380 := E380 + 1;
      Gnoga.Server.Database.Sqlite'Elab_Spec;
      Gnoga.Server.Database.Sqlite'Elab_Body;
      E382 := E382 + 1;
      v22'elab_spec;
      v22.fls'elab_spec;
      v22.prg'elab_spec;
      v22.sql'elab_spec;
      E239 := E239 + 1;
      v22.msg'elab_spec;
      E252 := E252 + 1;
      E255 := E255 + 1;
      E250 := E250 + 1;
      E295 := E295 + 1;
      E257 := E257 + 1;
      E263 := E263 + 1;
      E280 := E280 + 1;
      E394 := E394 + 1;
      E396 := E396 + 1;
      E398 := E398 + 1;
      E400 := E400 + 1;
      v22.cfg'elab_spec;
      E386 := E386 + 1;
      E207 := E207 + 1;
      E390 := E390 + 1;
      E392 := E392 + 1;
      E388 := E388 + 1;
      v22.net'elab_spec;
      E402 := E402 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_testapi");

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
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_msg.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_sql.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_sys.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_tio.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-cfg.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_cfg.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-crl.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-crl-callbacks.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi_crl.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/v22-net.o
   --   /home/sr/Sowebio/informatique/dev/gpl/github/v22/obj/debug/testapi.o
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
   --   -ldl
--  END Object file/option list   

end ada_main;
