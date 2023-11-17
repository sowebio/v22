------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      s-memory.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   EUPL 1.2
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library bind to GNATColl.Memory
--
--  @description
--  Monitoring memory usage
--
--  GnatColl.pdf & gnatcoll-memory.ads
--  https://www.adacore.com/gems/gem-78
--  You need the three above sources to get full information usage
--
--  Activate       GNATCOLL.Memory.Configure (Activate_Monitor => True);
--  Deactivate     GNATCOLL.Memory.Configure (Activate_Monitor => False);
--  Dump           GNATCOLL.Memory.Dump (Size => 3, Report => Memory_Usage); ¹
--  Reset counters GNATCOLL.Memory.Reset; ²
--
--  ¹ Print on the console three backtraces for the code that allocated the
--   most memory. Use addr2line -e to convert to a symbolic backtrace.
--  ² Use of Reset then Dump calls allows precise memory usage monitoring
--   in specific parts of the code
--
--  @authors
--  AdaCore
--  Stéphane Rivière - sr - sriviere@soweb.io (description)
--
--  @versions
--  20210325 - 0.1 - sr - v20 lib integration
------------------------------------------------------------------------------

with GNATCOLL.Memory;

package body System.Memory is

   package M renames GNATCOLL.Memory;

   function Alloc (Size : size_t) return System.Address is
   begin
      return M.Alloc (M.size_t (Size));
   end Alloc;

   procedure Free (Ptr : System.Address) renames M.Free;

   function Realloc
      (Ptr  : System.Address;
       Size : size_t)
      return System.Address
   is
   begin
      return M.Realloc (Ptr, M.size_t (Size));
   end Realloc;

------------------------------------------------------------------------------
end System.Memory;
------------------------------------------------------------------------------
