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
-- Generic procedure for dealling with property metadata                    --
------------------------------------------------------------------------------

package body KOW_Ent.Generic_Property_Metadata is



	procedure Set(
			Property	: in out Entity_Property_Type'Class;
			Metadata	: in     Metadata_Access
		) is
	begin
		Property.Metadata( My_Index ) := Entity_Property_Metadata_Access( Metadata );
	end Set;



	function Get(
			Property : in Entity_Property_type'Class
		) return Metadata_Access is
	begin
		if Property.Metadata( My_Index ) = null then
			return On_Null( Property );
		else
			return Metadata_Access( Property.Metadata( My_Index ) );
		end if;
	end Get;
	

begin
	KOW_Ent.Metadata_Registry.Allocate( My_Index );
end KOW_Ent.Generic_Property_Metadata;
	
