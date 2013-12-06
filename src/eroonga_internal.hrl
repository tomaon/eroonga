%% =============================================================================
%% Copyright 2013 AONO Tomohiko
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

%% -- define --
-define(GRN_CTX_MORE,  (1 bsl 0)).
-define(GRN_CTX_TAIL,  (1 bsl 1)).
-define(GRN_CTX_HEAD,  (1 bsl 2)).
-define(GRN_CTX_QUIET, (1 bsl 3)).
-define(GRN_CTX_QUIT,  (1 bsl 4)).

%% -- enum: grn_content_type --
-define(GRN_CONTENT_NONE,    0).
-define(GRN_CONTENT_TSV,     1).
-define(GRN_CONTENT_JSON,    2).
-define(GRN_CONTENT_XML,     3).
-define(GRN_CONTENT_MSGPACK, 4).

%% == ~/lib/com.h ==

%% -- define --
-define(GRN_COM_PROTO_GQTP, "\xc7").


%% == other ==

-define(ISSET(B,P), (P =:= B band P)).

-type(property() :: proplists:property()).
-type(socket() :: port()).                      % gen_tcp:socket()
