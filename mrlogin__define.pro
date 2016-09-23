; docformat = 'rst'
;
; NAME:
;       MrLogin__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Create a username and password login widget.
;
; :Categories:
;   Web Utility
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
;       2015/09/05  -   Written by Matthew Argall
;       2016/03/05  -   Pressing return from password box is like clicking "OK". - MRA
;       2016/05/29  -   Center log-in widget on screen. Added WINDOW_TITLE keyword. - MRA
;-
;*****************************************************************************************
;+
;   Create and realize the login widget.
;-
pro MrLogin::Create_GUI, $
GROUP_LEADER=group_leader, $
WINDOW_TITLE=window_title
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------
	;Default title
	if n_elements(window_title) eq 0 then window_title = 'Login'

	;Find the center of the screen
	xy_screen = get_screen_size()
	xoffset = xy_screen[0]/2
	yoffset = xy_screen[1]/2

	;Make a top-level base with or without a groupleader. cdf_read_gui2 is called by other
	;blocking widgets, so if a group_leader is given, then make cdf_read_gui2 modal.
	if n_elements(group_leader) ne 0 then begin
		no_block = 0
		tlb = widget_base(GROUP_LEADER=group_leader, TITLE=window_title, /COLUMN, $
		                  XOFFSET=xoffset, YOFFSET=yoffset, UNAME='tlb', /BASE_ALIGN_CENTER, $
		                  /MODAL)
	endif else begin
		no_block = 0
		tlb = widget_base(TITLE=window_title, /COLUMN, XOFFSET=xoffset, YOFFSET=yoffset, $
		                  UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse

	self.tlb = tlb

;---------------------------------------------------------------------
;Required Fields Column //////////////////////////////////////////////
;---------------------------------------------------------------------
	;USERNAME
	wbase = widget_base(tlb, COLUMN=2)
	label = widget_label(wbase, VALUE='username:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='USERNAME', VALUE=self.username, /EDITABLE, XSIZE=form_width)

	;PASSWORD
	wbase = widget_base(tlb, COLUMN=2)
	label = widget_label(wbase, VALUE='password:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='PASSWORD', VALUE=password, /EDITABLE, XSIZE=form_width, $
	                    /ALL_EVENTS, EVENT_PRO='MrLogin_Events', $
	                    UVALUE={object: self, method: 'Hide_Password'})

;---------------------------------------------------------------------
;Create OK and Cancel and "Show All" Buttons /////////////////////////
;---------------------------------------------------------------------
	okBase = widget_base(tlb, ROW=1)
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='OK', VALUE='OK', $
	                       UVALUE={object: self, method:'Ok'}, XSIZE=10*!D.X_CH_Size)
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='CANCEL', VALUE='Cancel', $
	                       UVALUE={object: self, method:'Cancel'}, XSIZE=10*!D.X_CH_Size)

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /REALIZE
	
	;Call XMANAGER
	xmanager, 'MrLogin', tlb, cleanup='MrLogin_Cleanup', $
	          event_handler='MrLogin_Events', NO_BLOCK=no_block
end


;+
;   Destroy the object.
;-
pro MrLogin::Destroy
	compile_opt idl2
	on_error, 2

	;Destroy the widget
	widget_control, self.tlb, /DESTROY
end


;+
;   Get object properties
;
; :Keywords:
;       USERNAME:           out, optional, type=string
;                           Username.
;-
pro MrLogin::GetProperty, $
CANCEL   = cancel, $
USERNAME = username, $
PASSWORD = password
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	if arg_present(cancel)   gt 0 then cancel   = self.cancel
	if arg_present(username) gt 0 then username = self.username
	if arg_present(password) gt 0 then password = self.password
end


;+
;   Handle events from the 'OK' button.
;-
pro MrLogin::Hide_Password, event
	compile_opt idl2
	
	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
	    catch, /cancel
	    void = cgErrorMsg()
	    return
	endif

;---------------------------------------------------------------------
;Insert Single Characters ////////////////////////////////////////////
;---------------------------------------------------------------------

	if event.type eq 0 then begin
		len = strlen(self.password)

		;Let the enter/return key indicate user has finished
		;   - 10B = Line Feed (Unix)
		;   - 13B = Carriage Return (Windows)
		if event.ch eq 10B || event.ch eq 13B then begin
			self -> OK, event
			return
		endif

		;Insert the character where it belongs
		case event.offset of
			1:    self.password = string(event.ch) + self.password
			len:  self.password = self.password + string(event.ch)
			else: self.password = strmid(self.password, 0, event.offset-1) + $
			                      string(event.ch) + $
			                      strmid(self.password, event.offset-1, len)
		endcase
	endif

;---------------------------------------------------------------------
;Insert Multiple Characters //////////////////////////////////////////
;---------------------------------------------------------------------

	if event.type eq 1 then begin
		len = strlen(self.password)

		;Insert the character where it belongs
		case event.offset of
			1:    self.password = event.str + self.password
			len:  self.password = self.password + event.str
			else: self.password = strmid(self.password, 0, event.offset-1) + $
			                      event.str + $
			                      strmid(self.password, event.offset-1, len)
		endcase
	endif

;---------------------------------------------------------------------
;Delete Text /////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	if event.type eq 2 then begin
		len = strlen(self.password)

		;Insert the character where it belongs
		self.password = strmid(self.password, 0, event.offset) + $
		                strmid(self.password, event.offset + event.length, len)
	endif

;---------------------------------------------------------------------
;Replace Password with "x"'s /////////////////////////////////////////
;---------------------------------------------------------------------
	
	if event.type eq 0 || event.type eq 1 || event.type eq 2 then begin
		;Get the password's length and the widget ID
		out_len = strlen(self.password)
		pwdID = widget_info(self.tlb, FIND_BY_UNAME='PASSWORD')
	
		;Replace with "x"'s
		if out_len eq 0 $
			then widget_control, pwdID, SET_VALUE='' $
			else widget_control, pwdID, SET_VALUE=replicate('*', out_len)
		
		;Set the cursor position.
		widget_control, pwdID, SET_TEXT_SELECT=event.offset
	endif
	
end


;+
;   Handle events from the 'OK' button.
;-
pro MrLogin::Ok, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Get the widget IDs of the username widget
	unameID  = widget_info(self.tlb, FIND_BY_UNAME='USERNAME')

	;Get its values
	widget_control, unameID,  GET_VALUE=username

	;Save the username. Do not set the password because the text widget is 
	;filled with "x"'s.
	self -> SetProperty, USERNAME = username
	
	;Destroy the widget
	self -> Destroy
end


;+
;   Handle events from the 'OK' button.
;-
pro MrLogin::Cancel, event
	compile_opt idl2
	on_error, 2
	
	;Indicate that the user cancelled
	self.cancel = 1B

	;Destroy the widget and the object
	self -> Destroy
end


;+
;   Set object properties
;
; :Keywords:
;       USERNAME:           in, optional, type=string
;                           Username.
;       PASSWORD:           in, optional, type=string
;                           Password.
;-
pro MrLogin::SetProperty, $
USERNAME = username, $
PASSWORD = password
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Set properties
	if n_elements(username) ne 0 then self.username = username
	if n_elements(password) ne 0 then self.password = password
end


;+
;   Forward events to the proper event handling method.
;-
pro MrLogin_Events, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Destroy the widget
	widget_control, event.id, GET_UVALUE=event_handler
	if event_handler.method ne '' then call_method, event_handler.method, event_handler.object, event

end


;+
;   Cleanup after the widget is destroyed.
;-
pro MrLogin_Cleanup, tlb
	;Nothing to do.
end


;+
;   Cleanup after the object is destroyed. This will destroy the widget, if it exists.
;-
pro MrLogin::Cleanup
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Erase the password
	self.password = ''
end


;+
;   Create a widget to ask for login information.
;
; :Keywords:
;       GROUP_LEADER:       in, optional, type=any
;                           Widget ID of the widget group leader.
;       PASSWORD:           in, optional, type=string, default=''
;                           Login password.
;       USERNAME:           in, optional, type=string, default=''
;                           Login username.
;       WINDOW_TITLE:       in, optional, type=string, default='Login'
;                           Text to be displayed on the log-in window's title bar.
;-
function MrLogin::init, $
GROUP_LEADER = group_leader, $
USERNAME = username, $
PASSWORD = password, $
WINDOOW_TITLE=window_title
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, 0
	endif

	;Set username and password
	if n_elements(username) gt 0 then self.username = username
	if n_elements(password) gt 0 then self.password = password

	;Ask for username and password
	self -> Create_GUI, GROUP_LEADER=group_leader, WINDOW_TITLE=window_title

	return, 1
end


;+
;   Object definition.
;
; :Fields:
;       TLB:                Top level base for the widget.
;       USERNAME:           Login username.
;       PASSWORD:           Login password.
;       CANCEL:             Indicates that the login event was cancelled.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrLogin__Define, class
	compile_opt idl2
	
	class = { MrLogin, $
	          inherits IDL_Object, $
	          tlb:      0L, $
	          cancel:   0B, $
	          username: '', $
	          password: ''  $
	        }
end