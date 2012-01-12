------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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
-- Main property types for KOW Ent                                          --
------------------------------------------------------------------------------



package KOW_Ent.Properties is


	type Integer_Property(
				Name		: access String;
				Container	: KOW_Ent.Property_Container_Ptr
			) is new KOW_Ent.Property_Type( Name, Container, KOW_Ent.APQ_Integer, 0 ) with null record;

	type Real_Property(
				Name		: access String;
				Container	: KOW_Ent.Property_Container_Ptr
			) is new KOW_Ent.Property_Type( Name, Container, KOW_Ent.APQ_Real, 0 ) with null record;
	

	type String_Property(
				Name		: access String;
				Container	: KOW_Ent.Property_Container_Ptr;
				String_Length	: Positive
			) is new KOW_Ent.Property_Type( Name, Container, KOW_Ent.APQ_String, String_Length ) with null record;

end KOW_Ent.Properties;
