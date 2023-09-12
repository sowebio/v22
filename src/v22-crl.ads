-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-crl.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - curl interface
--
--  @description
--
-- System packages for dynamic link only : sudo apt install libcurl4 libcurl4-openssl-dev
-- Dynamic linking (ok) : Common_Linker_Options := ("-lcurl");
-- standard lib location: /usr/lib/x86_64-linux-gnu/
-- from standard debian libcurl: pkg-config --libs --static libcurl
-- -L/usr/lib/x86_64-linux-gnu/mit-krb5 -lcurl -lnghttp2 -lidn2 -lrtmp -lpsl -lssl -lcrypto \
--    -lssl -lcrypto -Wl,-Bsymbolic-functions -Wl,-z,relro -lgssapi_krb5 -lkrb5 -lk5crypto -lcom_err -llber -lldap -llber -lz 
--
-- In test.adb, before 'begin', add :
--  Curl : Crl.CURL_P;
--  Res  : Crl.Curlcode;
--
-- In test.adb, above 'raise exception test'
--  Curl := Crl.Curl_Easy_Init;
--  if Curl /= null then
--     Res := Crl.Curl_Easy_Setopt(Curl,Crl.CURLOPT_URL,New_String("https://www.soweb.io"));
--     Res := Crl.Curl_Easy_Perform(Curl);
--     Crl.Curl_Easy_Cleanup(Curl);
--  end if;
--
--  An Ada binary, statically compiled, lies 2,9 Mo:
--
--  The same Ada binary, dynamically compiled, lies 1,9 MB and owns theses dependencies:
--
--  linux-vdso.so.1 (0x00007ffddfeba000)
--  libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fb2f30b0000)
--  libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fb2f2cbf000)
--  /lib64/ld-linux-x86-64.so.2 (0x00007fb2f32b4000)
--
--  The same binary, dynamically compiled, with the standard curl library,
--  thus with rather rich functionalities has, apart from the 4 libs above and the libcurl,
--  38 dependencies, in relation with the caps specified by the ./configure during the build:
--
--  libcurl.so.4 => /usr/lib/x86_64-linux-gnu/libcurl.so.4 (0x00007f772aec2000)
--
--  38 dependencies:
--    libnghttp2.so.14 => /usr/lib/x86_64-linux-gnu/libnghttp2.so.14 (0x00007f772a6a8000)
--    libidn2.so.0 => /usr/lib/x86_64-linux-gnu/libidn2.so.0 (0x00007f772a48b000)
--    librtmp.so.1 => /usr/lib/x86_64-linux-gnu/librtmp.so.1 (0x00007f772a26f000)
--    libpsl.so.5 => /usr/lib/x86_64-linux-gnu/libpsl.so.5 (0x00007f772a061000)
--    libssl.so.1.1 => /usr/lib/x86_64-linux-gnu/libssl.so.1.1 (0x00007f7729dd4000)
--    libcrypto.so.1.1 => /usr/lib/x86_64-linux-gnu/libcrypto.so.1.1 (0x00007f7729909000)
--    libgssapi_krb5.so.2 => /usr/lib/x86_64-linux-gnu/libgssapi_krb5.so.2 (0x00007f77296be000)
--    libldap_r-2.4.so.2 => /usr/lib/x86_64-linux-gnu/libldap_r-2.4.so.2 (0x00007f772946c000)
--    liblber-2.4.so.2 => /usr/lib/x86_64-linux-gnu/liblber-2.4.so.2 (0x00007f772925e000)
--    libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f7729041000)
--    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f7728e22000)
--    libunistring.so.2 => /usr/lib/x86_64-linux-gnu/libunistring.so.2 (0x00007f7728aa4000)
--    libgnutls.so.30 => /usr/lib/x86_64-linux-gnu/libgnutls.so.30 (0x00007f772873e000)
--    libhogweed.so.4 => /usr/lib/x86_64-linux-gnu/libhogweed.so.4 (0x00007f7728508000)
--    libnettle.so.6 => /usr/lib/x86_64-linux-gnu/libnettle.so.6 (0x00007f77282d2000)
--    libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f7728051000)
--    libkrb5.so.3 => /usr/lib/x86_64-linux-gnu/libkrb5.so.3 (0x00007f7727d7b000)
--    libk5crypto.so.3 => /usr/lib/x86_64-linux-gnu/libk5crypto.so.3 (0x00007f7727b49000)
--    libcom_err.so.2 => /lib/x86_64-linux-gnu/libcom_err.so.2 (0x00007f7727945000)
--    libkrb5support.so.0 => /usr/lib/x86_64-linux-gnu/libkrb5support.so.0 (0x00007f772773a000)
--    libresolv.so.2 => /lib/x86_64-linux-gnu/libresolv.so.2 (0x00007f7727520000)
--    libsasl2.so.2 => /usr/lib/x86_64-linux-gnu/libsasl2.so.2 (0x00007f7727305000)
--    libgssapi.so.3 => /usr/lib/x86_64-linux-gnu/libgssapi.so.3 (0x00007f77270c4000)
--    libp11-kit.so.0 => /usr/lib/x86_64-linux-gnu/libp11-kit.so.0 (0x00007f7726d95000)
--    libtasn1.so.6 => /usr/lib/x86_64-linux-gnu/libtasn1.so.6 (0x00007f7726b82000)
--    libkeyutils.so.1 => /lib/x86_64-linux-gnu/libkeyutils.so.1 (0x00007f772697e000)
--    libheimntlm.so.0 => /usr/lib/x86_64-linux-gnu/libheimntlm.so.0 (0x00007f7726775000)
--    libkrb5.so.26 => /usr/lib/x86_64-linux-gnu/libkrb5.so.26 (0x00007f77264e8000)
--    libasn1.so.8 => /usr/lib/x86_64-linux-gnu/libasn1.so.8 (0x00007f7726246000)
--    libhcrypto.so.4 => /usr/lib/x86_64-linux-gnu/libhcrypto.so.4 (0x00007f7726010000)
--    libroken.so.18 => /usr/lib/x86_64-linux-gnu/libroken.so.18 (0x00007f7725dfa000)
--    libffi.so.6 => /usr/lib/x86_64-linux-gnu/libffi.so.6 (0x00007f7725bf2000)
--    libwind.so.0 => /usr/lib/x86_64-linux-gnu/libwind.so.0 (0x00007f77259c9000)
--    libheimbase.so.1 => /usr/lib/x86_64-linux-gnu/libheimbase.so.1 (0x00007f77257ba000)
--    libhx509.so.5 => /usr/lib/x86_64-linux-gnu/libhx509.so.5 (0x00007f7725570000)
--    libsqlite3.so.0 => /usr/lib/x86_64-linux-gnu/libsqlite3.so.0 (0x00007f7725267000)
--    libcrypt.so.1 => /lib/x86_64-linux-gnu/libcrypt.so.1 (0x00007f772502f000)
--    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f7724c91000)
--
--  @authors
--  Andreas Almroth - aa - https://web.archive.org/web/20070403105909/http://www.almroth.com/adacurl (author)
--  Stéphane Rivière - sr - sriviere@soweb.io (low level sources amalgation, debugging, rewriting, and high level functions)
--
--  @versions
--  See git log
------------------------------------------------------------------------------

with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Calendar; use Ada.Calendar;

with v22.Uxs; use v22.Uxs;

package v22.Crl is

   subtype Time_T is long;
   type Time_T_Pointer is access all Time_T;

   type CURL is new System.Address; -- void
   type CURL_P is access all CURL;

   subtype File_P is System.Address;

   HTTPPOST_FILENAME    : constant long := 1;
   HTTPPOST_READFILE    : constant long := 2;
   HTTPPOST_PTRNAME     : constant long := 4;
   HTTPPOST_PTRCONTENTS : constant long := 8;
   HTTPPOST_BUFFER      : constant long := 16;
   HTTPPOST_PTRBUFFER   : constant long := 32;

   type Curl_Slist;
   type Curl_Slist_P is access all Curl_Slist;

   type Curl_Slist is record
      Data : chars_ptr;
      Next : Curl_Slist_P;
   end record;
   pragma Convention(C,Curl_Slist);

   type Curl_Httppost;
   type Curl_Httppost_P is access all Curl_Httppost;

   type Curl_Httppost is record
      Next           : Curl_Httppost_P;
      Name           : chars_ptr;
      Namelength     : long;
      Contents       : chars_ptr;
      Contentslength : long;
      Buffer         : chars_ptr;
      Bufferlength   : long;
      Contenttype    : chars_ptr;
      Contentheader  : Curl_Slist_P;
      More           : Curl_Httppost_P;
      Flags          : long;
      Showfilename   : chars_ptr;
   end record;
   pragma Convention(C,Curl_Httppost);

   CURL_MAX_WRITE_SIZE : constant := 16384;

   type Curl_Progress_Callback is access function (Clientp : System.Address;
                                                   Dltotal : double;
                                                   Dlnow   : double;
                                                   Ultotal : double;
                                                   Ulnow   : double) return int;
   --
                      
   type Curl_Readwrite_Callback is access function (Buf      : chars_ptr;
                                                    Size     : size_t;
                                                    Nmemb    : size_t;
                                                    Stream   : File_P) return size_t;
   --

   CURL_READFUNC_ABORT : constant := 16#10000000#;

   --  -- Not used since 7.10.8, will be removed
   --  type Curl_Passwd_Callback
   --  is access function (Clientp : System.Address;
   --                      Prompt  : chars_ptr;
   --                      Buffer  : chars_ptr;
   --                      Buflen  : int) return int;

   type Curlioerr is (CURLIOE_OK, CURLIOE_UNKNOWNCMD, CURLIOE_FAILRESTART, CURLIOE_LAST);
   pragma Convention (C, Curlioerr);

   type Curliocmd is (CURLIOCMD_NOP, CURLIOCMD_RESTARTREAD, CURLIOCMD_LAST);

   type Curl_Ioctl_Callback   is access function (Handle  : CURL;
                                                  Cmd     : int;
                                                  Clientp : System.Address) return Curlioerr;
   --

   type Curl_Malloc_Callback is access function (Size : size_t) return System.Address;
   --
   
   type Curl_Free_Callback is access function (Ptr : System.Address) return System.Address;
   --
   
   type Curl_Realloc_Callback is access function (Ptr  : System.Address; Size : size_t) return System.Address;
   --
   
   type Curl_Strdup_Callback is access function (Str : in chars_ptr) return chars_ptr;
   --
   
   type Curl_Calloc_Callback is access function(Nmemb : size_t; Size  : size_t) return System.Address;
   --
   
   type Curl_Infotype is (CURLINFO_TEXT,
                          CURLINFO_HEADER_IN,
                          CURLINFO_HEADER_OUT,
                          CURLINFO_DATA_IN,
                          CURLINFO_DATA_OUT,
                          CURLINFO_SSL_DATA_IN,
                          CURLINFO_SSL_DATA_OUT,
                          CURLINFO_END);
   pragma Convention(C,Curl_Infotype);

   type Curl_Debug_Callback is access function (Handle : CURL_P;
                                                Ctype  : Curl_Infotype;
                                                Data   : chars_ptr;
                                                Size   : size_t;
                                                Userp  : System.Address) return int;
   --

   type Curlcode is (CURLE_OK,
                     CURLE_UNSUPPORTED_PROTOCOL,
                     CURLE_FAILED_INIT,
                     CURLE_URL_MALFORMAT,
                     CURLE_URL_MALFORMAT_USER,
                     CURLE_COULDNT_RESOLVE_PROXY,
                     CURLE_COULDNT_RESOLVE_HOST,
                     CURLE_COULDNT_CONNECT,
                     CURLE_FTP_WEIRD_SERVER_REPLY,
                     CURLE_FTP_ACCESS_DENIED,
                     CURLE_FTP_USER_PASSWORD_INCORRECT,
                     CURLE_FTP_WEIRD_PASS_REPLY,
                     CURLE_FTP_WEIRD_USER_REPLY,
                     CURLE_FTP_WEIRD_PASV_REPLY,
                     CURLE_FTP_WEIRD_227_FORMAT,
                     CURLE_FTP_CANT_GET_HOST,
                     CURLE_FTP_CANT_RECONNECT,
                     CURLE_FTP_COULDNT_SET_BINARY,
                     CURLE_PARTIAL_FILE,
                     CURLE_FTP_COULDNT_RETR_FILE,
                     CURLE_FTP_WRITE_ERROR,
                     CURLE_FTP_QUOTE_ERROR,
                     CURLE_HTTP_RETURNED_ERROR,
                     CURLE_WRITE_ERROR,
                     CURLE_MALFORMAT_USER,
                     CURLE_FTP_COULDNT_STOR_FILE,
                     CURLE_READ_ERROR,
                     CURLE_OUT_OF_MEMORY,
                     CURLE_OPERATION_TIMEDOUT,
                     CURLE_FTP_COULDNT_SET_ASCII,
                     CURLE_FTP_PORT_FAILED,
                     CURLE_FTP_COULDNT_USE_REST,
                     CURLE_FTP_COULDNT_GET_SIZE,
                     CURLE_HTTP_RANGE_ERROR,
                     CURLE_HTTP_POST_ERROR,
                     CURLE_SSL_CONNECT_ERROR,
                     CURLE_BAD_DOWNLOAD_RESUME,
                     CURLE_FILE_COULDNT_READ_FILE,
                     CURLE_LDAP_CANNOT_BIND,
                     CURLE_LDAP_SEARCH_FAILED,
                     CURLE_LIBRARY_NOT_FOUND,
                     CURLE_FUNCTION_NOT_FOUND,
                     CURLE_ABORTED_BY_CALLBACK,
                     CURLE_BAD_FUNCTION_ARGUMENT,
                     CURLE_BAD_CALLING_ORDER,
                     CURLE_HTTP_PORT_FAILED,
                     CURLE_BAD_PASSWORD_ENTERED,
                     CURLE_TOO_MANY_REDIRECTS ,
                     CURLE_UNKNOWN_TELNET_OPTION,
                     CURLE_TELNET_OPTION_SYNTAX ,
                     CURLE_OBSOLETE,
                     CURLE_SSL_PEER_CERTIFICATE,
                     CURLE_GOT_NOTHING,
                     CURLE_SSL_ENGINE_NOTFOUND,
                     CURLE_SSL_ENGINE_SETFAILED,
                     CURLE_SEND_ERROR,
                     CURLE_RECV_ERROR,
                     CURLE_SHARE_IN_USE,
                     CURLE_SSL_CERTPROBLEM,
                     CURLE_SSL_CIPHER,
                     CURLE_SSL_CACERT,
                     CURLE_BAD_CONTENT_ENCODING,
                     CURLE_LDAP_INVALID_URL,
                     CURLE_FILESIZE_EXCEEDED,
                     CURLE_FTP_SSL_FAILED,
                     CURLE_SEND_FAIL_REWIND,
                     CURLE_SSL_ENGINE_INITFAILED,
                     CURLE_LOGIN_DENIED,
                     CURL_LAST);
   pragma Convention(C,Curlcode);

   type Curl_Ssl_Ctx_Callback is access function (C       : CURL_P;
                                                  Ssl_Ctx : System.Address;
                                                  Userptr : System.Address) return Curlcode;
   --

   type Curl_Proxytype is (CURLPROXY_HTTP,
                           CURLPROXY_SOCKS4,
                           CURLPROXY_SOCKS5);
                           
   for Curl_Proxytype use (CURLPROXY_HTTP => 0,
                           CURLPROXY_SOCKS4 => 4,
                           CURLPROXY_SOCKS5 => 5);
   pragma Convention(C,Curl_Proxytype);


   CURLAUTH_NONE         : constant := 0;
   CURLAUTH_BASIC        : constant := 1;
   CURLAUTH_DIGEST       : constant := 2;
   CURLAUTH_GSSNEGOTIATE : constant := 4;
   CURLAUTH_NTLM         : constant := 8;
   CURLAUTH_ANY          : constant := 16#ffffffff#; -- invert 0, assume 32 bit
   CURLAUTH_ANYSAFE      : constant := 16#fffffffe#; -- assume 32 bit (~CURLAUTH_BASIC)

   CURL_ERROR_SIZE : constant := 256;

   type Curl_Ftpssl is (CURLFTPSSL_NONE,
                        CURLFTPSSL_TRY,
                        CURLFTPSSL_CONTROL,
                        CURLFTPSSL_ALL,
                        CURLFTPSSL_LAST);
   pragma Convention(C,Curl_Ftpssl);

   type  Curl_Ftpauth is (CURLFTPAUTH_DEFAULT,
                          CURLFTPAUTH_SSL,
                          CURLFTPAUTH_TLS,
                          CURLFTPAUTH_LAST);
   pragma Convention(C,Curl_Ftpauth);

   CURLOPTTYPE_LONG          : constant := 0;
   CURLOPTTYPE_OBJECTPOINT   : constant := 10000;
   CURLOPTTYPE_FUNCTIONPOINT : constant := 20000;
   CURLOPTTYPE_OFF_T         : constant := 30000;

   type Curloption is (CURLOPT_PORT,
                       CURLOPT_TIMEOUT,
                       CURLOPT_INFILESIZE,
                       CURLOPT_LOW_SPEED_LIMIT,
                       CURLOPT_LOW_SPEED_TIME,
                       CURLOPT_RESUME_FROM,
                       CURLOPT_CRLF,
                       CURLOPT_SSLVERSION,
                       CURLOPT_TIMECONDITION,
                       CURLOPT_TIMEVALUE,
                       CURLOPT_VERBOSE,
                       CURLOPT_HEADER,
                       CURLOPT_NOPROGRESS,
                       CURLOPT_NOBODY,
                       CURLOPT_FAILONERROR,
                       CURLOPT_UPLOAD,
                       CURLOPT_POST,
                       CURLOPT_FTPLISTONLY,
                       CURLOPT_FTPAPPEND,
                       CURLOPT_NETRC,
                       CURLOPT_FOLLOWLOCATION,
                       CURLOPT_TRANSFERTEXT,
                       CURLOPT_PUT,
                       CURLOPT_AUTOREFERER,
                       CURLOPT_PROXYPORT,
                       CURLOPT_POSTFIELDSIZE,
                       CURLOPT_HTTPPROXYTUNNEL,
                       CURLOPT_SSL_VERIFYPEER,
                       CURLOPT_MAXREDIRS,
                       CURLOPT_FILETIME,
                       CURLOPT_MAXCONNECTS,
                       CURLOPT_CLOSEPOLICY,
                       CURLOPT_FRESH_CONNECT,
                       CURLOPT_FORBID_REUSE,
                       CURLOPT_CONNECTTIMEOUT,
                       CURLOPT_HTTPGET,
                       CURLOPT_SSL_VERIFYHOST,
                       CURLOPT_HTTP_VERSION,
                       CURLOPT_FTP_USE_EPSV,
                       CURLOPT_SSLENGINE_DEFAULT,
                       CURLOPT_DNS_USE_GLOBAL_CACHE, -- Obsolete soon
                       CURLOPT_DNS_CACHE_TIMEOUT,
                       CURLOPT_COOKIESESSION,
                       CURLOPT_BUFFERSIZE,
                       CURLOPT_NOSIGNAL,
                       CURLOPT_PROXYTYPE,
                       CURLOPT_UNRESTRICTED_AUTH,
                       CURLOPT_FTP_USE_EPRT,
                       CURLOPT_HTTPAUTH,
                       CURLOPT_FTP_CREATE_MISSING_DIRS,
                       CURLOPT_PROXYAUTH,
                       CURLOPT_FTP_RESPONSE_TIMEOUT,
                       CURLOPT_IPRESOLVE,
                       CURLOPT_MAXFILESIZE,
                       CURLOPT_FTP_SSL,
                       CURLOPT_TCP_NODELAY,
                       CURLOPT_FTPSSLAUTH,
                       CURLOPT_FILE,
                       CURLOPT_URL,
                       CURLOPT_PROXY,
                       CURLOPT_USERPWD,
                       CURLOPT_PROXYUSERPWD,
                       CURLOPT_RANGE,
                       CURLOPT_INFILE,
                       CURLOPT_ERRORBUFFER,
                       CURLOPT_POSTFIELDS,
                       CURLOPT_REFERER,
                       CURLOPT_FTPPORT,
                       CURLOPT_USERAGENT,
                       CURLOPT_COOKIE,
                       CURLOPT_HTTPHEADER,
                       CURLOPT_HTTPPOST,
                       CURLOPT_SSLCERT,
                       CURLOPT_SSLKEYPASSWD,
                       CURLOPT_QUOTE,
                       CURLOPT_WRITEHEADER,
                       CURLOPT_COOKIEFILE,
                       CURLOPT_CUSTOMREQUEST,
                       CURLOPT_STDERR,
                       CURLOPT_POSTQUOTE,
                       CURLOPT_WRITEINFO,
                       CURLOPT_PROGRESSDATA,
                       CURLOPT_INTERFACE,
                       CURLOPT_KRB4LEVEL,
                       CURLOPT_CAINFO,
                       CURLOPT_TELNETOPTIONS,
                       CURLOPT_RANDOM_FILE,
                       CURLOPT_EGDSOCKET,
                       CURLOPT_COOKIEJAR,
                       CURLOPT_SSL_CIPHER_LIST,
                       CURLOPT_SSLCERTTYPE,
                       CURLOPT_SSLKEY,
                       CURLOPT_SSLKEYTYPE,
                       CURLOPT_SSLENGINE,
                       CURLOPT_PREQUOTE,
                       CURLOPT_DEBUGDATA,
                       CURLOPT_CAPATH,
                       CURLOPT_SHARE,
                       CURLOPT_ENCODING,
                       CURLOPT_PRIVATE,
                       CURLOPT_HTTP200ALIASES,
                       CURLOPT_SSL_CTX_DATA,
                       CURLOPT_NETRC_FILE,
                       CURLOPT_SOURCE_USERPWD,
                       CURLOPT_SOURCE_PREQUOTE,
                       CURLOPT_SOURCE_POSTQUOTE,
                       CURLOPT_IOCTLDATA,
                       CURLOPT_SOURCE_URL,
                       CURLOPT_SOURCE_QUOTE,
                       CURLOPT_FTP_ACCOUNT,
                       CURLOPT_WRITEFUNCTION,
                       CURLOPT_READFUNCTION,
                       CURLOPT_PROGRESSFUNCTION,
                       CURLOPT_HEADERFUNCTION,
                       CURLOPT_DEBUGFUNCTION,
                       CURLOPT_SSL_CTX_FUNCTION,
                       CURLOPT_IOCTLFUNCTION,
                       CURLOPT_INFILESIZE_LARGE,
                       CURLOPT_RESUME_FROM_LARGE,
                       CURLOPT_MAXFILESIZE_LARGE,
                       CURLOPT_POSTFIELDSIZE_LARGE,
                       CURLOPT_LASTENTRY); -- what value?

   for Curloption use (CURLOPT_PORT => (CURLOPTTYPE_LONG + 3),
                       CURLOPT_TIMEOUT => (CURLOPTTYPE_LONG + 13),
                       CURLOPT_INFILESIZE => (CURLOPTTYPE_LONG + 14),
                       CURLOPT_LOW_SPEED_LIMIT => (CURLOPTTYPE_LONG  + 19),
                       CURLOPT_LOW_SPEED_TIME => (CURLOPTTYPE_LONG + 20),
                       CURLOPT_RESUME_FROM => (CURLOPTTYPE_LONG + 21),
                       CURLOPT_CRLF => (CURLOPTTYPE_LONG + 27),
                       CURLOPT_SSLVERSION => (CURLOPTTYPE_LONG + 32),
                       CURLOPT_TIMECONDITION => (CURLOPTTYPE_LONG + 33),
                       CURLOPT_TIMEVALUE => (CURLOPTTYPE_LONG + 34),
                       CURLOPT_VERBOSE => (CURLOPTTYPE_LONG + 41),
                       CURLOPT_HEADER => (CURLOPTTYPE_LONG + 42),
                       CURLOPT_NOPROGRESS => (CURLOPTTYPE_LONG + 43),
                       CURLOPT_NOBODY => (CURLOPTTYPE_LONG + 44),
                       CURLOPT_FAILONERROR => (CURLOPTTYPE_LONG + 45),
                       CURLOPT_UPLOAD => (CURLOPTTYPE_LONG + 46),
                       CURLOPT_POST => (CURLOPTTYPE_LONG + 47),
                       CURLOPT_FTPLISTONLY => (CURLOPTTYPE_LONG + 48),
                       CURLOPT_FTPAPPEND => (CURLOPTTYPE_LONG + 50),
                       CURLOPT_NETRC => (CURLOPTTYPE_LONG + 51),
                       CURLOPT_FOLLOWLOCATION => (CURLOPTTYPE_LONG + 52),
                       CURLOPT_TRANSFERTEXT => (CURLOPTTYPE_LONG + 53),
                       CURLOPT_PUT => (CURLOPTTYPE_LONG + 54),
                       CURLOPT_AUTOREFERER => (CURLOPTTYPE_LONG + 58),
                       CURLOPT_PROXYPORT => (CURLOPTTYPE_LONG + 59),
                       CURLOPT_POSTFIELDSIZE => (CURLOPTTYPE_LONG + 60),
                       CURLOPT_HTTPPROXYTUNNEL => (CURLOPTTYPE_LONG + 61),
                       CURLOPT_SSL_VERIFYPEER => (CURLOPTTYPE_LONG + 64),
                       CURLOPT_MAXREDIRS => (CURLOPTTYPE_LONG + 68),
                       CURLOPT_FILETIME => (CURLOPTTYPE_LONG + 69),
                       CURLOPT_MAXCONNECTS => (CURLOPTTYPE_LONG + 71),
                       CURLOPT_CLOSEPOLICY => (CURLOPTTYPE_LONG + 72),
                       CURLOPT_FRESH_CONNECT => (CURLOPTTYPE_LONG + 74),
                       CURLOPT_FORBID_REUSE => (CURLOPTTYPE_LONG + 75),
                       CURLOPT_CONNECTTIMEOUT => (CURLOPTTYPE_LONG + 78),
                       CURLOPT_HTTPGET => (CURLOPTTYPE_LONG + 80),
                       CURLOPT_SSL_VERIFYHOST => (CURLOPTTYPE_LONG + 81),
                       CURLOPT_HTTP_VERSION => (CURLOPTTYPE_LONG + 84),
                       CURLOPT_FTP_USE_EPSV => (CURLOPTTYPE_LONG + 85),
                       CURLOPT_SSLENGINE_DEFAULT => (CURLOPTTYPE_LONG + 90),
                       CURLOPT_DNS_USE_GLOBAL_CACHE =>(CURLOPTTYPE_LONG + 91),
                       CURLOPT_DNS_CACHE_TIMEOUT => (CURLOPTTYPE_LONG + 92),
                       CURLOPT_COOKIESESSION => (CURLOPTTYPE_LONG + 96),
                       CURLOPT_BUFFERSIZE => (CURLOPTTYPE_LONG + 98),
                       CURLOPT_NOSIGNAL => (CURLOPTTYPE_LONG + 99),
                       CURLOPT_PROXYTYPE => (CURLOPTTYPE_LONG + 101),
                       CURLOPT_UNRESTRICTED_AUTH => (CURLOPTTYPE_LONG + 105),
                       CURLOPT_FTP_USE_EPRT => (CURLOPTTYPE_LONG + 106),
                       CURLOPT_HTTPAUTH => (CURLOPTTYPE_LONG + 107),
                       CURLOPT_FTP_CREATE_MISSING_DIRS => (CURLOPTTYPE_LONG + 110),
                       CURLOPT_PROXYAUTH => (CURLOPTTYPE_LONG + 111),
                       CURLOPT_FTP_RESPONSE_TIMEOUT => (CURLOPTTYPE_LONG + 112),
                       CURLOPT_IPRESOLVE => (CURLOPTTYPE_LONG + 113),
                       CURLOPT_MAXFILESIZE => (CURLOPTTYPE_LONG + 114),
                       CURLOPT_FTP_SSL => (CURLOPTTYPE_LONG + 119),
                       CURLOPT_TCP_NODELAY => (CURLOPTTYPE_LONG + 121),
                       CURLOPT_FTPSSLAUTH => (CURLOPTTYPE_LONG + 129),
                       CURLOPT_FILE => (CURLOPTTYPE_OBJECTPOINT + 1),
                       CURLOPT_URL => (CURLOPTTYPE_OBJECTPOINT + 2),
                       CURLOPT_PROXY => (CURLOPTTYPE_OBJECTPOINT + 4),
                       CURLOPT_USERPWD => (CURLOPTTYPE_OBJECTPOINT + 5),
                       CURLOPT_PROXYUSERPWD => (CURLOPTTYPE_OBJECTPOINT + 6),
                       CURLOPT_RANGE => (CURLOPTTYPE_OBJECTPOINT + 7),
                       CURLOPT_INFILE => (CURLOPTTYPE_OBJECTPOINT + 9),
                       CURLOPT_ERRORBUFFER => (CURLOPTTYPE_OBJECTPOINT + 10),
                       CURLOPT_POSTFIELDS => (CURLOPTTYPE_OBJECTPOINT + 15),
                       CURLOPT_REFERER => (CURLOPTTYPE_OBJECTPOINT + 16),
                       CURLOPT_FTPPORT => (CURLOPTTYPE_OBJECTPOINT + 17),
                       CURLOPT_USERAGENT => (CURLOPTTYPE_OBJECTPOINT + 18),
                       CURLOPT_COOKIE => (CURLOPTTYPE_OBJECTPOINT + 22),
                       CURLOPT_HTTPHEADER => (CURLOPTTYPE_OBJECTPOINT + 23),
                       CURLOPT_HTTPPOST => (CURLOPTTYPE_OBJECTPOINT + 24),
                       CURLOPT_SSLCERT => (CURLOPTTYPE_OBJECTPOINT + 25),
                       CURLOPT_SSLKEYPASSWD => (CURLOPTTYPE_OBJECTPOINT + 26),
                       CURLOPT_QUOTE => (CURLOPTTYPE_OBJECTPOINT + 28),
                       CURLOPT_WRITEHEADER => (CURLOPTTYPE_OBJECTPOINT + 29),
                       CURLOPT_COOKIEFILE => (CURLOPTTYPE_OBJECTPOINT + 31),
                       CURLOPT_CUSTOMREQUEST =>(CURLOPTTYPE_OBJECTPOINT + 36),
                       CURLOPT_STDERR => (CURLOPTTYPE_OBJECTPOINT + 37),
                       CURLOPT_POSTQUOTE => (CURLOPTTYPE_OBJECTPOINT + 39),
                       CURLOPT_WRITEINFO => (CURLOPTTYPE_OBJECTPOINT + 40),
                       CURLOPT_PROGRESSDATA => (CURLOPTTYPE_OBJECTPOINT + 57),
                       CURLOPT_INTERFACE => (CURLOPTTYPE_OBJECTPOINT + 62),
                       CURLOPT_KRB4LEVEL => (CURLOPTTYPE_OBJECTPOINT + 63),
                       CURLOPT_CAINFO => (CURLOPTTYPE_OBJECTPOINT + 65),
                       CURLOPT_TELNETOPTIONS =>(CURLOPTTYPE_OBJECTPOINT + 70),
                       CURLOPT_RANDOM_FILE => (CURLOPTTYPE_OBJECTPOINT + 76),
                       CURLOPT_EGDSOCKET => (CURLOPTTYPE_OBJECTPOINT + 77),
                       CURLOPT_COOKIEJAR => (CURLOPTTYPE_OBJECTPOINT + 82),
                       CURLOPT_SSL_CIPHER_LIST=>(CURLOPTTYPE_OBJECTPOINT +83),
                       CURLOPT_SSLCERTTYPE => (CURLOPTTYPE_OBJECTPOINT + 86),
                       CURLOPT_SSLKEY => (CURLOPTTYPE_OBJECTPOINT + 87),
                       CURLOPT_SSLKEYTYPE => (CURLOPTTYPE_OBJECTPOINT + 88),
                       CURLOPT_SSLENGINE => (CURLOPTTYPE_OBJECTPOINT + 89),
                       CURLOPT_PREQUOTE => (CURLOPTTYPE_OBJECTPOINT + 93),
                       CURLOPT_DEBUGDATA => (CURLOPTTYPE_OBJECTPOINT + 95),
                       CURLOPT_CAPATH => (CURLOPTTYPE_OBJECTPOINT + 97),
                       CURLOPT_SHARE => (CURLOPTTYPE_OBJECTPOINT + 100),
                       CURLOPT_ENCODING => (CURLOPTTYPE_OBJECTPOINT + 102),
                       CURLOPT_PRIVATE => (CURLOPTTYPE_OBJECTPOINT + 103),
                       CURLOPT_HTTP200ALIASES=>(CURLOPTTYPE_OBJECTPOINT +104),
                       CURLOPT_SSL_CTX_DATA=>(CURLOPTTYPE_OBJECTPOINT +109),
                       CURLOPT_NETRC_FILE=>(CURLOPTTYPE_OBJECTPOINT + 118),
                       CURLOPT_SOURCE_USERPWD => (CURLOPTTYPE_OBJECTPOINT + 123),
                       CURLOPT_SOURCE_PREQUOTE => (CURLOPTTYPE_OBJECTPOINT + 127),
                       CURLOPT_SOURCE_POSTQUOTE=>(CURLOPTTYPE_OBJECTPOINT+128),
                       CURLOPT_IOCTLDATA => (CURLOPTTYPE_OBJECTPOINT + 131),
                       CURLOPT_SOURCE_URL => (CURLOPTTYPE_OBJECTPOINT + 132),
                       CURLOPT_SOURCE_QUOTE => (CURLOPTTYPE_OBJECTPOINT + 133),
                       CURLOPT_FTP_ACCOUNT => (CURLOPTTYPE_OBJECTPOINT + 134),
                       CURLOPT_WRITEFUNCTION=>(CURLOPTTYPE_FUNCTIONPOINT +11),
                       CURLOPT_READFUNCTION=>(CURLOPTTYPE_FUNCTIONPOINT + 12),
                       CURLOPT_PROGRESSFUNCTION=>(CURLOPTTYPE_FUNCTIONPOINT+56),
                       CURLOPT_HEADERFUNCTION=>(CURLOPTTYPE_FUNCTIONPOINT + 79),
                       CURLOPT_DEBUGFUNCTION=>(CURLOPTTYPE_FUNCTIONPOINT + 94),
                       CURLOPT_SSL_CTX_FUNCTION => (CURLOPTTYPE_FUNCTIONPOINT + 108),
                       CURLOPT_IOCTLFUNCTION => (CURLOPTTYPE_FUNCTIONPOINT + 130),
                       CURLOPT_INFILESIZE_LARGE => (CURLOPTTYPE_OFF_T + 115),
                       CURLOPT_RESUME_FROM_LARGE => (CURLOPTTYPE_OFF_T + 116),
                       CURLOPT_MAXFILESIZE_LARGE => (CURLOPTTYPE_OFF_T + 117),
                       CURLOPT_POSTFIELDSIZE_LARGE => (CURLOPTTYPE_OFF_T + 120),
                       CURLOPT_LASTENTRY => (CURLOPTTYPE_OFF_T + 121)
                       );
   pragma Convention(C,Curloption);

   CURL_IPRESOLVE_WHATEVER : constant := 0;
   CURL_IPRESOLVE_V4       : constant := 1;
   CURL_IPRESOLVE_V6       : constant := 2;

   CURLOPT_WRITEDATA  : constant Curloption := CURLOPT_FILE;
   CURLOPT_READDATA   : constant Curloption := CURLOPT_INFILE;
   CURLOPT_HEADERDATA : constant Curloption := CURLOPT_WRITEHEADER;

   type Curl_Http_Version is (CURL_HTTP_VERSION_NONE,
                              CURL_HTTP_VERSION_1_0,
                              CURL_HTTP_VERSION_1_1,
                              CURL_HTTP_VERSION_LAST);
   pragma Convention(C,Curl_Http_Version);

   type Curl_Netrc_Option is (CURL_NETRC_IGNORED,
                              CURL_NETRC_OPTIONAL,
                              CURL_NETRC_REQUIRED,
                              CURL_NETRC_LAST);
   pragma Convention(C,Curl_Netrc_Option);

   type Curl_Ssl_Version is (CURL_SSLVERSION_DEFAULT,
                             CURL_SSLVERSION_TLSv1,
                             CURL_SSLVERSION_SSLv2,
                             CURL_SSLVERSION_SSLv3,
                             CURL_SSLVERSION_LAST);
   pragma Convention(C,Curl_Ssl_Version);

   type Curl_Timecond is (CURL_TIMECOND_NONE,
                          CURL_TIMECOND_IFMODSINCE,
                          CURL_TIMECOND_IFUNMODSINCE,
                          CURL_TIMECOND_LASTMOD,
                          CURL_TIMECOND_LAST);
   pragma Convention(C,Curl_Timecond);

   type Curlformoption is (CURLFORM_NOTHING,
                           CURLFORM_COPYNAME,
                           CURLFORM_PTRNAME,
                           CURLFORM_NAMELENGTH,
                           CURLFORM_COPYCONTENTS,
                           CURLFORM_PTRCONTENTS,
                           CURLFORM_CONTENTSLENGTH,
                           CURLFORM_FILECONTENT,
                           CURLFORM_ARRAY,
                           CURLFORM_OBSOLETE,
                           CURLFORM_FILE,
                           CURLFORM_BUFFER,
                           CURLFORM_BUFFERPTR,
                           CURLFORM_BUFFERLENGTH,
                           CURLFORM_CONTENTTYPE,
                           CURLFORM_CONTENTHEADER,
                           CURLFORM_FILENAME,
                           CURLFORM_END,
                           CURLFORM_OBSOLETE2,
                           CURLFORM_LASTENTRY);
   pragma Convention(C,Curlformoption);

   type Curl_Forms is record
      Option : Curlformoption;
      Value  : chars_ptr;
   end record;

   type Curlformcode is (CURL_FORMADD_OK,
                         CURL_FORMADD_MEMORY,
                         CURL_FORMADD_OPTION_TWICE,
                         CURL_FORMADD_NULL,
                         CURL_FORMADD_UNKNOWN_OPTION,
                         CURL_FORMADD_INCOMPLETE,
                         CURL_FORMADD_ILLEGAL_ARRAY,
                         CURL_FORMADD_DISABLED,
                         CURL_FORMADD_LAST);
   pragma Convention(C,Curlformcode);
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : chars_ptr) return Curlformcode;
   --
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : long;
                          E1 : Curlformoption) return Curlformcode;
   --
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : chars_ptr;
                          C2 : Curlformoption;
                          D2 : chars_ptr;
                          E1 : Curlformoption) return Curlformcode;
   --
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : chars_ptr;
                          C2 : Curlformoption;
                          D2 : long;
                          E1 : Curlformoption) return Curlformcode;
   --
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : long;
                          C2 : Curlformoption;
                          D2 : long;
                          E1 : Curlformoption) return Curlformcode;
   --
   
   function Curl_Formadd (Httppost  : access Curl_Httppost_P;
                          Last_Post : access Curl_Httppost_P;
                          C1 : Curlformoption;
                          D1 : long;
                          C2 : Curlformoption;
                          D2 : chars_ptr;
                          E1 : Curlformoption) return Curlformcode;
   --
   
   procedure Curl_Formfree(Form : Curl_Httppost_P);
   --
   
   function Curl_Getenv(Variable : chars_ptr) return chars_ptr;
   --
   
   function Curl_Version return chars_ptr;
   --
   function Curl_Escape(String : chars_ptr; Length : int) return chars_ptr;
   --
   
   function Curl_Unescape(String : chars_ptr; Length : int) return chars_ptr;
   --
   
   procedure Curl_Free(P : System.Address);
   --
   
   function Curl_Global_Init(Flags : long) return Curlcode;
   --
   
   -- Added sr@20211029 to avoid warnings like below...
   -- 706 subprogram pointer "Curl_Global_Init_Mem.M" should have foreign convention [-gnatwx]
   -- 706 add Convention pragma to declaration of "Adacurl.Curl_Malloc_Callback" at line 146 [-gnatwx]
   pragma Convention(C,Curl_Malloc_Callback);
   pragma Convention(C,Curl_Free_Callback);
   pragma Convention(C,Curl_Realloc_Callback);
   pragma Convention(C,Curl_Strdup_Callback);
   pragma Convention(C,Curl_Calloc_Callback);

   function Curl_Global_Init_Mem(Flags : long;
                                 M     : Curl_Malloc_Callback;
                                 F     : Curl_Free_Callback;
                                 R     : Curl_Realloc_Callback;
                                 S     : Curl_Strdup_Callback;
                                 C     : Curl_Calloc_Callback)
                                return Curlcode;
   --
   
   procedure Curl_Global_Cleanup;
   --
   
   function Curl_Slist_Append (A1 : Curl_Slist_P;
                               A2 : chars_ptr)
                              return Curl_Slist_P;
   --
   
   procedure Curl_Slist_Free_All(A1 : Curl_Slist_P);
   --
   
   function Curl_Getdate(P   : chars_ptr;
                         Now : Time_T_Pointer)
                        return Time_T;
   --
   
   CURLINFO_STRING   : constant := 16#100000#;
   CURLINFO_LONG     : constant := 16#200000#;
   CURLINFO_DOUBLE   : constant := 16#300000#;
   CURLINFO_SLIST    : constant := 16#400000#;
   CURLINFO_MASK     : constant := 16#0fffff#;
   CURLINFO_TYPEMASK : constant := 16#f00000#;

   type Curlinfo is (CURLINFO_NONE,
                     CURLINFO_EFFECTIVE_URL,
                     CURLINFO_CONTENT_TYPE,
                     CURLINFO_PRIVATE,
                     CURLINFO_RESPONSE_CODE,
                     CURLINFO_HEADER_SIZE,
                     CURLINFO_REQUEST_SIZE,
                     CURLINFO_SSL_VERIFYRESULT,
                     CURLINFO_FILETIME,
                     CURLINFO_REDIRECT_COUNT,
                     CURLINFO_HTTP_CONNECTCODE,
                     CURLINFO_HTTPAUTH_AVAIL,
                     CURLINFO_PROXYAUTH_AVAIL,
                     CURLINFO_OS_ERRNO,
                     CURLINFO_NUM_CONNECTS,
                     CURLINFO_TOTAL_TIME,
                     CURLINFO_NAMELOOKUP_TIME,
                     CURLINFO_CONNECT_TIME,
                     CURLINFO_PRETRANSFER_TIME,
                     CURLINFO_SIZE_UPLOAD,
                     CURLINFO_SIZE_DOWNLOAD,
                     CURLINFO_SPEED_DOWNLOAD,
                     CURLINFO_SPEED_UPLOAD,
                     CURLINFO_CONTENT_LENGTH_DOWNLOAD,
                     CURLINFO_CONTENT_LENGTH_UPLOAD,
                     CURLINFO_STARTTRANSFER_TIME,
                     CURLINFO_REDIRECT_TIME,
                     CURLINFO_SSL_ENGINES,
                     CURLINFO_LASTONE);
                     
   for Curlinfo use (CURLINFO_NONE                    => 0,
                     CURLINFO_EFFECTIVE_URL           => CURLINFO_STRING + 1,
                     CURLINFO_CONTENT_TYPE            => CURLINFO_STRING + 18,
                     CURLINFO_PRIVATE                 => CURLINFO_STRING + 21,
                     CURLINFO_RESPONSE_CODE           => CURLINFO_LONG   + 2,
                     CURLINFO_HEADER_SIZE             => CURLINFO_LONG   + 11,
                     CURLINFO_REQUEST_SIZE            => CURLINFO_LONG   + 12,
                     CURLINFO_SSL_VERIFYRESULT        => CURLINFO_LONG   + 13,
                     CURLINFO_FILETIME                => CURLINFO_LONG   + 14,
                     CURLINFO_REDIRECT_COUNT          => CURLINFO_LONG   + 20,
                     CURLINFO_HTTP_CONNECTCODE        => CURLINFO_LONG   + 22,
                     CURLINFO_HTTPAUTH_AVAIL          => CURLINFO_LONG   + 23,
                     CURLINFO_PROXYAUTH_AVAIL         => CURLINFO_LONG   + 24,
                     CURLINFO_OS_ERRNO                => CURLINFO_LONG   + 25,
                     CURLINFO_NUM_CONNECTS            => CURLINFO_LONG   + 26,
                     CURLINFO_TOTAL_TIME              => CURLINFO_DOUBLE + 3,
                     CURLINFO_NAMELOOKUP_TIME         => CURLINFO_DOUBLE + 4,
                     CURLINFO_CONNECT_TIME            => CURLINFO_DOUBLE + 5,
                     CURLINFO_PRETRANSFER_TIME        => CURLINFO_DOUBLE + 6,
                     CURLINFO_SIZE_UPLOAD             => CURLINFO_DOUBLE + 7,
                     CURLINFO_SIZE_DOWNLOAD           => CURLINFO_DOUBLE + 8,
                     CURLINFO_SPEED_DOWNLOAD          => CURLINFO_DOUBLE + 9,
                     CURLINFO_SPEED_UPLOAD            => CURLINFO_DOUBLE + 10,
                     CURLINFO_CONTENT_LENGTH_DOWNLOAD => CURLINFO_DOUBLE + 15,
                     CURLINFO_CONTENT_LENGTH_UPLOAD   => CURLINFO_DOUBLE + 16,
                     CURLINFO_STARTTRANSFER_TIME      => CURLINFO_DOUBLE + 17,
                     CURLINFO_REDIRECT_TIME           => CURLINFO_DOUBLE + 19,
                     CURLINFO_SSL_ENGINES             => CURLINFO_SLIST + 27,
                     CURLINFO_LASTONE                 => CURLINFO_SLIST + 28);
   pragma Convention (C, Curlinfo);

   type Curl_Closepolicy is (CURLCLOSEPOLICY_NONE,
                             CURLCLOSEPOLICY_OLDEST,
                             CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
                             CURLCLOSEPOLICY_LEAST_TRAFFIC,
                             CURLCLOSEPOLICY_SLOWEST,
                             CURLCLOSEPOLICY_CALLBACK,
                             CURLCLOSEPOLICY_LAST);
   pragma Convention (C, Curl_Closepolicy);

   CURL_GLOBAL_SSL     : constant := 1;
   CURL_GLOBAL_WIN32   : constant := 2;
   CURL_GLOBAL_ALL     : constant := CURL_GLOBAL_SSL + CURL_GLOBAL_WIN32;
   CURL_GLOBAL_NOTHING : constant := 0;
   CURL_GLOBAL_DEFAULT : constant := CURL_GLOBAL_ALL;

   type Curl_Lock_Data is (CURL_LOCK_DATA_NONE,
                           CURL_LOCK_DATA_SHARE,
                           CURL_LOCK_DATA_COOKIE,
                           CURL_LOCK_DATA_DNS,
                           CURL_LOCK_DATA_SSL_SESSION,
                           CURL_LOCK_DATA_CONNECT,
                           CURL_LOCK_DATA_LAST);
   pragma Convention (C, Curl_Lock_Data);

   type Curl_Lock_Access is (CURL_LOCK_ACCESS_NONE,
                             CURL_LOCK_ACCESS_SHARED,
                             CURL_LOCK_ACCESS_SINGLE,
                             CURL_LOCK_ACCESS_LAST);
   pragma Convention (C, Curl_Lock_Access);

   type Curl_Lock_Function is access procedure (Handle : CURL_P;
                                                Data : Curl_Lock_Data;
                                                Locktype : Curl_Lock_Access;
                                                Userptr : System.Address);

   type Curl_Unlock_Function is access procedure (Handle : CURL_P;
                                                  Data : Curl_Lock_Data;
                                                  Userptr : System.Address);

   type CURLSH is new System.Address;
   type CURLSH_P is access all CURLSH;

   type CURLSHcode is (CURLSHE_OK,
                       CURLSHE_BAD_OPTION,
                       CURLSHE_IN_USE,
                       CURLSHE_INVALID,
                       CURLSHE_LAST);

   type CURLSHoption is (CURLSHOPT_NONE,
                         CURLSHOPT_SHARE,
                         CURLSHOPT_UNSHARE,
                         CURLSHOPT_LOCKFUNC,
                         CURLSHOPT_UNLOCKFUNC,
                         CURLSHOPT_USERDATA,
                         CURLSHOPT_LAST);

   function Curl_Share_Init return CURLSH_P;
   --
   
   function Curl_Share_Setopt(H : CURLSH_P; Option : CURLSHoption; D : System.Address) return CURLSHcode;
   --
   
   -- Added sr@20211029 to avoid warnings
   pragma Convention(C,Curl_Lock_Function);
   pragma Convention(C,Curl_Unlock_Function);

   function Curl_Share_Setopt(H : CURLSH_P; Option : CURLSHoption; D : Curl_Lock_Function) return CURLSHcode;
   --
   
   function Curl_Share_Setopt(H : CURLSH_P; Option : CURLSHoption; D : Curl_Unlock_Function) return CURLSHcode;
   --

   function Curl_Share_Cleanup(H : CURLSH_P) return CURLSHcode;
   --
   
   type Curlversion is (CURLVERSION_FIRST, CURLVERSION_SECOND, CURLVERSION_THIRD, CURLVERSION_LAST);
   pragma Convention (C, Curlversion);

   CURLVERSION_NOW : constant Curlversion := CURLVERSION_THIRD;

   type Curl_Version_Info_Data is record
      Age : Curlversion;
      Version : chars_ptr;
      Version_Num : unsigned;
      Host : chars_ptr;
      Features : int;
      Ssl_Version : chars_ptr;
      Ssl_Version_Num : long;
      Libz_Version : chars_ptr;
      Protocols : chars_ptr;
      ares : chars_ptr;
      Ares_Num : int;
      Libidn : chars_ptr;
   end record;
   pragma Convention(C,Curl_Version_Info_Data);

   type Curl_Version_Info_Data_P is access all Curl_Version_Info_Data;

   CURL_VERSION_IPV6         : constant := 1;
   CURL_VERSION_KERBEROS4    : constant := 2;
   CURL_VERSION_SSL          : constant := 4;
   CURL_VERSION_LIBZ         : constant := 8;
   CURL_VERSION_NTLM         : constant := 16;
   CURL_VERSION_GSSNEGOTIATE : constant := 32;
   CURL_VERSION_DEBUG        : constant := 64;
   CURL_VERSION_ASYNCHDNS    : constant := 128;
   CURL_VERSION_SPNEGO       : constant := 256;
   CURL_VERSION_LARGEFILE    : constant := 512;
   CURL_VERSION_IDN          : constant := 1024;
   CURL_VERSION_SSPI         : constant := 2048;

   function Curl_Version_Info(A1 : Curlversion) return Curl_Version_Info_Data_P;
   --
   
   function Curl_Easy_Strerror(C : Curlcode) return chars_ptr;
   --
   
   function Curl_Share_Strerror(C : CURLSHcode) return chars_ptr;

   ---------------------------------------------------------------------------
   -- cURL easy low level functions
   ---------------------------------------------------------------------------

   function Curl_Easy_Init return CURL_P;

   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : chars_ptr) return Curlcode;
   --
   
   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : long) return Curlcode;
   --
   
   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : double) return Curlcode;

   -- Added sr@20211029 to avoid warnings
   pragma Convention (C, Curl_Readwrite_Callback);
   --pragma Convention (C, Curl_Passwd_Callback);
   pragma Convention (C, Curl_Progress_Callback);
   pragma Convention (C, Curl_Ssl_Ctx_Callback);

   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : Curl_Readwrite_Callback) return Curlcode;
   --function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : Curl_Passwd_Callback) return Curlcode;
   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : Curl_Progress_Callback) return Curlcode;
   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : Curl_Ssl_Ctx_Callback) return Curlcode;
   function Curl_Easy_Setopt(C : CURL_P; Option : Curloption; Datap  : File_P) return Curlcode;
   --

   function Curl_Easy_Perform(C : CURL_P) return Curlcode;
   --
   
   procedure Curl_Easy_Cleanup(C : CURL_P);
   --
   
   procedure Curl_Easy_Reset(C : CURL_P);
   --

   function Curl_Easy_Getinfo(C : CURL_P; Option : Curlinfo; Datap  : access chars_ptr) return Curlcode;
   --
   
   function Curl_Easy_Getinfo(C : CURL_P; Option : Curlinfo; Datap  : access long) return Curlcode;
   --
   
   function Curl_Easy_Getinfo(C : CURL_P; Option : Curlinfo; Datap  : access double) return Curlcode;
   --
   
   function Curl_Easy_Duphandle(C : CURL_P) return CURL_P;
   --

   ---------------------------------------------------------------------------
   -- cURL multi low level functions
   ---------------------------------------------------------------------------

   subtype CURLM is System.Address;
   type CURLM_P is access all CURLM;

   type CURLMcode is (CURLM_CALL_MULTI_PERFORM,
                      CURLM_OK,
                      CURLM_BAD_HANDLE,
                      CURLM_BAD_EASY_HANDLE,
                      CURLM_OUT_OF_MEMORY,
                      CURLM_INTERNAL_ERROR,
                      CURLM_LAST);

   for CURLMcode use (CURLM_CALL_MULTI_PERFORM => -1,
                      CURLM_OK                 => 0,
                      CURLM_BAD_HANDLE         => 1,
                      CURLM_BAD_EASY_HANDLE    => 2,
                      CURLM_OUT_OF_MEMORY      => 3,
                      CURLM_INTERNAL_ERROR     => 4,
                      CURLM_LAST               => 5);
   pragma Convention(C,CURLMcode);

   type CURL_MSG is (CURLMSG_NONE, CURLMSG_DONE, CURLMSG_LAST);
   pragma Convention(C,CURL_MSG);

   type Data_Union_Range is new Positive range 1..2;

   type Data_Union (Which : Data_Union_Range := 1) is record
      case Which is
         when 1 =>
            Whatever : System.Address;
         when 2 =>
            Result : Curlcode;
      end case;
   end record;
   pragma Unchecked_Union(Data_Union);

   type Curlmsg is record
      Msg         : CURL_MSG;
      Easy_Handle : CURL_P;
      Data        : Data_Union;
   end record;
   pragma Convention(C,Curlmsg);
   
   type Curlmsg_P is access all Curlmsg;
   pragma Convention(C,Curlmsg_P);

   function Curl_Multi_Init return CURLM_P;
   --

   function Curl_Multi_Add_Handle(Multi_Handle : CURLM_P; Curl_Handle  : CURLM_P) return CURLMcode;
   --
   
   function Curl_Multi_Remove_Handle(Multi_Handle : CURLM_P; Curl_Handle  : CURLM_P) return CURLMcode;
   --
   
   -- Use POSIX style fd_set
   ALIGNMENT : constant := Natural'Min (Standard'Maximum_Alignment,8);
   type Fd_Set is array (1 .. 32) of int;
   for Fd_Set'Alignment use ALIGNMENT;
   for Fd_Set'Size use 1024;
   pragma Convention (C,Fd_Set);

   type Fd_Set_P is access all Fd_Set;
   pragma Convention (C, Fd_Set_P);

   function Curl_Multi_Fdset (Multi_Handle : CURLM_P;
                              Read_Fd_Set  : Fd_Set;
                              Write_Fd_Set : Fd_Set;
                              Exc_Fd_Se    : Fd_Set;
                              Max_Fd       : access int) return CURLMcode;
   --
   
   function Curl_Multi_Perform(Multi_Handle : CURLM_P; Running_Handles : access int) return CURLMcode;
   --
   
   function Curl_Multi_Cleanup(Multi_Handle : CURLM_P) return CURLMcode;
   --
   
   function Curl_Multi_Info_Read(Multi_Handle  : CURLM_P; Msgs_In_Queue : access int) return Curlmsg_P;
   --
   
   function Curl_Multi_Strerror(C : CURLMcode) return chars_ptr;
   --
   
   ----------------------------------------------------------------------------
   function Get_Version return String;
   --  libcURL version (format : libcurl/7.81.0 OpenSSL/3.0.2 zlib/1.2.11 
   --  brotli/1.0.9 zstd/1.4.8 libidn2/2.3.2 libpsl/0.21.0 (+libidn2/2.3.2) 
   --  libssh/0.9.6/openssl/zlib nghttp2/1.43.0 librtmp/2.3 OpenLDAP/2.5.16).
   
-------------------------------------------------------------------------------
private

   pragma Import (C, Curl_Formadd, "curl_formadd");
   pragma Import (C, Curl_Formfree, "curl_formfree");
   pragma Import (C, Curl_Getenv, "curl_getenv");
   pragma Import (C, Curl_Version, "curl_version");
   pragma Import (C, Curl_Escape, "curl_escape");
   pragma Import (C, Curl_Unescape, "curl_unescape");
   pragma Import (C, Curl_Free, "curl_free");
   pragma Import (C, Curl_Global_Init, "curl_global_init");
   pragma Import (C, Curl_Global_Init_Mem, "curl_global_init_mem");
   pragma Import (C, Curl_Global_Cleanup, "curl_global_cleanup");
   pragma Import (C, Curl_Slist_Append, "curl_slist_append");
   pragma Import (C, Curl_Slist_Free_All, "curl_slist_free_all");
   pragma Import (C, Curl_Getdate, "curl_getdate");
   pragma Import (C, Curl_Version_Info, "curl_version_info");
   pragma Import (C, Curl_Share_Init, "curl_share_init");
   pragma Import (C, Curl_Share_Setopt, "curl_share_setopt");
   pragma Import (C, Curl_Share_Cleanup, "curl_share_cleanup");
   pragma Import (C, Curl_Easy_Strerror, "Curl_Easy_Strerror");
   pragma Import (C, Curl_Share_Strerror, "curl_share_strerror");

   pragma Import (C, Curl_Easy_Init, "curl_easy_init");
   pragma Import (C, Curl_Easy_Setopt, "curl_easy_setopt");
   pragma Import (C, Curl_Easy_Perform, "curl_easy_perform");
   pragma Import (C, Curl_Easy_Cleanup, "curl_easy_cleanup");
   pragma Import (C, Curl_Easy_Reset, "curl_easy_reset");
   pragma Import (C, Curl_Easy_Getinfo, "curl_easy_getinfo");
   pragma Import (C, Curl_Easy_Duphandle, "curl_easy_duphandle");

   pragma Import (C, Curl_Multi_Init, "curl_multi_Init");
   pragma Import (C, Curl_Multi_Add_Handle, "curl_multi_add_handle");
   pragma Import (C, Curl_Multi_Cleanup, "curl_multi_cleanup");
   pragma Import (C, Curl_Multi_Fdset, "curl_multi_fdset");
   pragma Import (C, Curl_Multi_Info_Read, "curl_multi_info_read");
   pragma Import (C, Curl_Multi_Perform, "curl_multi_perform");
   pragma Import (C, Curl_Multi_Remove_Handle, "curl_multi_remove_handle");
   pragma Import (C, Curl_Multi_Strerror, "curl_multi_strerror");

-------------------------------------------------------------------------------
end v22.Crl;
-------------------------------------------------------------------------------
