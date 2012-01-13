------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Contains methods for representing a logic operations in a query          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Unchecked_Deallocation;

package KOW_Ent.Queries.Logic_Operations is


	---------------------
	-- Stored vs Value --
	---------------------


	overriding
	procedure Adjust( Operation : in out Stored_Vs_Value_Operation ) is
	begin
		if Operation.Value /= null then
			Operation.Value := new Value_Type'( Operation.Value.all );
		end if;
	end Adjust;


	overriding
	procedure Finalize( Operation : in out Stored_Vs_Value_Operation ) is
	begin
		if Operation.Value /= null then
			Free( Operation.Value );
		end if;
	end Finalize;


	overriding
	procedure Free(
				Operation	: in     Stored_Vs_Value_Operation;
				Name		: in out Logic_Operation_Ptr
			) is
		procedure iFree is new Ada.Unchecked_Deallocation(
							Object	=> Stored_Vs_Value_Operation,
							Name	=> Stored_Vs_Value_Ptr
						);
	begin
		iFree( Stored_Vs_Value_Ptr ( Name ) );
	end Free;

	
	overriding
	function Clone(
				Operation	: in    Stored_Vs_Value_Operation 
			) return Logic_Operation_Ptr is
		Value : Stored_Vs_Value_Ptr := new Logic_Criteria_Operation'( Operation );
	begin
		return Logic_Operation_Ptr( Value );
	end Clone;


	----------------------
	-- Stored vs Stored --
	----------------------

	overriding
	procedure Free(
				Operation	: in     Stored_Vs_Stored_Operation;
				Name		: in out Logic_Operation_Ptr
			) is
		procedure iFree is new Ada.Unchecked_Deallocation(
							Object	=> Stored_Vs_Stored_Operation;
							Name	=> Stored_Vs_Stored_Ptr
						);
	begin
		iFree( Stored_Vs_Stored_Ptr( Name ) );
	end Free;

	
	overriding
	function Clone(
				Operation	: in     Stored_Vs_Stored_Operation
			) return Logic_Operation_Ptr is
		Value : Stored_Vs_Stored_Ptr := new Stored_Vs_Stored_Operation'( Operation );
	begin
		return Logic_Operation_Ptr( Value );
	end Clone;
	
	--------------------
	-- Logic_Criteria --
	--------------------

	type Logic_Criteria_Operation is new Logic_Operation_Type with record
		Logic_Criteria	: Logic_Criteria_Type;
		Appender	: Logic_Appender_Type	:= Appender_AND;
	end record;
	type Logic_Criteria_Ptr is access Logic_Criteria_Operation;


	overriding
	procedure Free(
				Operation	: in     Logic_Criteria_Operation;
				Name		: in out Logic_Operation_Ptr
			) is
		procedure iFree is new Ada.Unchecked_Deallocation(
							Object	=> Logic_Criteria_Operation;
							Name	=> Logic_Criteria_Ptr
						);
	begin
		iFree( Logic_Criteria_Ptr( Name ) );
	end Free;
	
	overriding
	function Clone(
				Operation	: in     Logic_Criteria_Operation
			) return Logic_Operation_Ptr
		Value : Logic_Criteria_Ptr := new Logic_Criteria_Operation'( Operation );
	begin
		return Logic_Operation_Ptr( Value );
	end Clone;

end KOW_Ent.Queries.Logic_Operations;
