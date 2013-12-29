%% =============================================================================
%% Copyright 2013-2014 AONO Tomohiko
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License version 2.1 as published by the Free Software Foundation.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%% =============================================================================

%% == ~/include/groonga.h ==

%% -- enumL grn_rc
-define(GRN_SUCCESS,                               0).
-define(GRN_END_OF_DATA,                           1).
-define(GRN_UNKNOWN_ERROR,                       - 1).
-define(GRN_OPERATION_NOT_PERMITTED,             - 2).
-define(GRN_NO_SUCH_FILE_OR_DIRECTORY,           - 3).
-define(GRN_NO_SUCH_PROCESS,                     - 4).
-define(GRN_INTERRUPTED_FUNCTION_CALL,           - 5).
-define(GRN_INPUT_OUTPUT_ERROR,                  - 6).
-define(GRN_NO_SUCH_DEVICE_OR_ADDRESS,           - 7).
-define(GRN_ARG_LIST_TOO_LONG,                   - 8).
-define(GRN_EXEC_FORMAT_ERROR,                   - 9).
-define(GRN_BAD_FILE_DESCRIPTOR,                 -10).
-define(GRN_NO_CHILD_PROCESSES,                  -11).
-define(GRN_RESOURCE_TEMPORARILY_UNAVAILABLE,    -12).
-define(GRN_NOT_ENOUGH_SPACE,                    -13).
-define(GRN_PERMISSION_DENIED,                   -14).
-define(GRN_BAD_ADDRESS,                         -15).
-define(GRN_RESOURCE_BUSY,                       -16).
-define(GRN_FILE_EXISTS,                         -17).
-define(GRN_IMPROPER_LINK,                       -18).
-define(GRN_NO_SUCH_DEVICE,                      -19).
-define(GRN_NOT_A_DIRECTORY,                     -20).
-define(GRN_IS_A_DIRECTORY,                      -21).
-define(GRN_INVALID_ARGUMENT,                    -22).
-define(GRN_TOO_MANY_OPEN_FILES_IN_SYSTEM,       -23).
-define(GRN_TOO_MANY_OPEN_FILES,                 -24).
-define(GRN_INAPPROPRIATE_I_O_CONTROL_OPERATION, -25).
-define(GRN_FILE_TOO_LARGE,                      -26).
-define(GRN_NO_SPACE_LEFT_ON_DEVICE,             -27).
-define(GRN_INVALID_SEEK,                        -28).
-define(GRN_READ_ONLY_FILE_SYSTEM,               -29).
-define(GRN_TOO_MANY_LINKS,                      -30).
-define(GRN_BROKEN_PIPE,                         -31).
-define(GRN_DOMAIN_ERROR,                        -32).
-define(GRN_RESULT_TOO_LARGE,                    -33).
-define(GRN_RESOURCE_DEADLOCK_AVOIDED,           -34).
-define(GRN_NO_MEMORY_AVAILABLE,                 -35).
-define(GRN_FILENAME_TOO_LONG,                   -36).
-define(GRN_NO_LOCKS_AVAILABLE,                  -37).
-define(GRN_FUNCTION_NOT_IMPLEMENTED,            -38).
-define(GRN_DIRECTORY_NOT_EMPTY,                 -39).
-define(GRN_ILLEGAL_BYTE_SEQUENCE,               -40).
-define(GRN_SOCKET_NOT_INITIALIZED,              -41).
-define(GRN_OPERATION_WOULD_BLOCK,               -42).
-define(GRN_ADDRESS_IS_NOT_AVAILABLE,            -43).
-define(GRN_NETWORK_IS_DOWN,                     -44).
-define(GRN_NO_BUFFER,                           -45).
-define(GRN_SOCKET_IS_ALREADY_CONNECTED,         -46).
-define(GRN_SOCKET_IS_NOT_CONNECTED,             -47).
-define(GRN_SOCKET_IS_ALREADY_SHUTDOWNED,        -48).
-define(GRN_OPERATION_TIMEOUT,                   -49).
-define(GRN_CONNECTION_REFUSED,                  -50).
-define(GRN_RANGE_ERROR,                         -51).
-define(GRN_TOKENIZER_ERROR,                     -52).
-define(GRN_FILE_CORRUPT,                        -53).
-define(GRN_INVALID_FORMAT,                      -54).
-define(GRN_OBJECT_CORRUPT,                      -55).
-define(GRN_TOO_MANY_SYMBOLIC_LINKS,             -56).
-define(GRN_NOT_SOCKET,                          -57).
-define(GRN_OPERATION_NOT_SUPPORTED,             -58).
-define(GRN_ADDRESS_IS_IN_USE,                   -59).
-define(GRN_ZLIB_ERROR,                          -60).
-define(GRN_LZO_ERROR,                           -61).
-define(GRN_STACK_OVER_FLOW,                     -62).
-define(GRN_SYNTAX_ERROR,                        -63).
-define(GRN_RETRY_MAX,                           -64).
-define(GRN_INCOMPATIBLE_FILE_FORMAT,            -65).
-define(GRN_UPDATE_NOT_ALLOWED,                  -66).
-define(GRN_TOO_SMALL_OFFSET,                    -67).
-define(GRN_TOO_LARGE_OFFSET,                    -68).
-define(GRN_TOO_SMALL_LIMIT,                     -69).
-define(GRN_CAS_ERROR,                           -70).
-define(GRN_UNSUPPORTED_COMMAND_VERSION,         -71).
-define(GRN_NORMALIZER_ERROR,                    -72).
