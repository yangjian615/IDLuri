; docformat = 'rst'
;
; NAME:
;       MrIsIPv6
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Check if a host name is an IPv6 address
;
; About:
;   A 128-bit IPv6 address is divided into eight 16-bit pieces.  Each
;   piece is represented numerically in case-insensitive hexadecimal,
;   using one to four hexadecimal digits (leading zeroes are permitted).
;   The eight encoded pieces are given most-significant first, separated
;   by colon characters.  Optionally, the least-significant two pieces
;   may instead be represented in IPv4 address textual format.  A
;   sequence of one or more consecutive zero-valued 16-bit pieces within
;   the address may be elided, omitting all their digits and leaving
;   exactly two consecutive colons in their place to mark the elision.
;
;      IPv6address =                            6( h16 ":" ) ls32
;                  /                       "::" 5( h16 ":" ) ls32
;                  / [               h16 ] "::" 4( h16 ":" ) ls32
;                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
;                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
;                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
;                  / [ *4( h16 ":" ) h16 ] "::"              ls32
;                  / [ *5( h16 ":" ) h16 ] "::"              h16
;                  / [ *6( h16 ":" ) h16 ] "::"
;
;      ls32        = ( h16 ":" h16 ) / IPv4address
;                  ; least-significant 32 bits of address
;
;      h16         = 1*4HEXDIG
;                  ; 16 bits of address represented in hexadecimal
;
; :Params:
;       HOST:           in, required, type=string/strarr
;                       An IPv6 address for a host.
;
; :Keywords:
;       REGEX:          out, optional, type=string
;                       The regular expression used to check for validity.
;
; :Returns:
;       ISIP64:         1 if `HOST` is an IPv4 addres, 0 otherwise.
;
; :Examples:
;   Test a complete IPv6 address
;       IDL> print, MrIsIPv6('FE80:0000:0000:0000:0202:B3FF:FE1E:8329')
;          1
;
;   Test a collapsed IPv6 address
;       IDL> print, MrIsIPv6('FE80::0202:B3FF:FE1E:8329')
;          1
;       
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014/09/05  -   Written by Matthew Argall
;-
function MrIsIPv6, host, $
REGEX=IPv6
	compile_opt idl2
	on_error, 2
	
	;IPv4
	octet  = '([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])'
	IPv4   = octet + '\.' + octet + '\.' + octet + '\.' + octet
	
	;IPv6 components
	h16  = '[0-9a-fA-F]{1,4}'
	ls32 = '(' + h16 + ':' + h16 + '|' + IPv4 + ')'
	
	;IPv6 regular expression
	IPv6 = '('                                                                                       + $
	                   '(' +                                      '(' + h16 + ':){7}'   +  h16 + ')' + $
	             '|' + '(' +                                      '(' + h16 + ':){6}'   + ls32 + ')' + $
	             '|' + '(' +                               '::' + '(' + h16 + ':){0,7}' +  h16 + ')' + $
	             '|' + '(' +                               '::' + '(' + h16 + ':){0,6}' + ls32 + ')' + $
	             '|' + '(' +                               '::' + '(' + h16 + ':){0,5}' + ls32 + ')' + $
	             '|' + '(' +                         h16 + '::' + '(' + h16 + ':){0,4}' + ls32 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,1}' + h16 + '::' + '(' + h16 + ':){0,3}' + ls32 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,2}' + h16 + '::' + '(' + h16 + ':){0,2}' + ls32 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,3}' + h16 + '::' + '(' + h16 + ':){0,1}' + ls32 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,4}' + h16 + '::'                         + ls32 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,5}' + h16 + '::'                         +  h16 + ')' + $
	             '|' + '(' + '(' + h16 + ':){0,6}' + h16 + '::'                                + ')' + $
	       ')'
	
	;Test
	isIPv6 = stregex(host, IPv6, /BOOLEAN)
	
	return, isIPv6
end


hosts = ['FE80:0000:0000:0000:0202:B3FF:FE1E:8329', $

         'FE80:0000:0000:0000:0202:B3FF:FE1E::', $
         'FE80:0000:0000:0000:0202:B3FF::8329', $
         'FE80:0000:0000:0000:0202::FE1E:8329', $
         'FE80:0000:0000:0000::B3FF:FE1E:8329', $
         'FE80:0000:0000::0202:B3FF:FE1E:8329', $
         'FE80:0000::0000:0202:B3FF:FE1E:8329', $
         'FE80::0000:0000:0202:B3FF:FE1E:8329', $
         '::FE80:0000:0000:0202:B3FF:FE1E:8329', $
         
         'FE80:0000:0000:0000:0202:8329::', $
         'FE80:0000:0000:0000:0202::8329', $
         'FE80:0000:0000:0000::FE1E:8329', $
         'FE80:0000:0000::B3FF:FE1E:8329', $
         'FE80:0000::0202:B3FF:FE1E:8329', $
         'FE80::0000:0202:B3FF:FE1E:8329', $
         '::FE80:0000:0202:B3FF:FE1E:8329', $
         
         'FE80:0000:0000:0000:0202::', $
         'FE80:0000:0000:0000::8329', $
         'FE80:0000:0000::FE1E:8329', $
         'FE80:0000::B3FF:FE1E:8329', $
         'FE80::0202:B3FF:FE1E:8329', $
         '::0000:0202:B3FF:FE1E:8329', $
         
         'FE80:0000:0000:0000::', $
         'FE80:0000:0000::8329', $
         'FE80:0000::FE1E:8329', $
         'FE80::B3FF:FE1E:8329', $
         '::0202:B3FF:FE1E:8329', $
         
         'FE80:0000:0000::', $
         'FE80:0000::8329', $
         'FE80::FE1E:8329', $
         '::B3FF:FE1E:8329', $
         
         'FE80:0000::', $
         'FE80::8329', $
         '::FE1E:8329', $
         
         'FE80::', $
         '::FE80' $
        ]

;Check if they pass the test
tf_ipv6 = MrIsIPv6(hosts)

;Print the results
print, 'PASS    IPv6'
for i = 0, n_elements(tf_ipv6) - 1 do $
	print, FORMAT='(%"  %1i     %s")', tf_ipv6[i], hosts[i]

end