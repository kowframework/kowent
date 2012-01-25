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
-- Package for creating SQL queries for table creation in KOW Ent           --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;				use KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;


package body KOW_Ent.SQL.Create is


	procedure Generate_Create(
			Generator	: in out Create_Generator_Type;
			Template_Entity	: in out KOW_Ent.Entity_Type'Class;
			Q		: in out APQ.Root_Query_Type
		) is
		-- generate the create query


		Is_First_Column : Boolean := True;
		procedure Column_Iterator( Property : in Property_Ptr ) is
		begin
			if Is_First_Column then
				Is_First_Column := False;
			else
				APQ.Append( Q, "," );
			end if;
			Append_Column_Specification(
					Generator	=> Create_Generator_Type'Class( Generator ),
					Property	=> Property,
					Q		=> Q
				);
		end Column_Iterator;

	begin
		APQ.Append( Q, "CREATE TABLE " & Trim( Get_Alias( Template_Entity ) ) & "(" );

		Iterate( Template_Entity, Column_Iterator'Access );

		if Generator.Id_Property /= null then
			APQ.Append( Q, "," );
			Append_ID_Specification(
					Generator	=> Create_Generator_Type'Class( Generator ),
					Q		=> Q
				);
		end if;
	end Generate_Create;

	

	--------------------------
	-- Column Specification --
	--------------------------
	procedure Append_Column_Specification(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Ptr;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		Val : Value_Type := Get_Value( Property.all );
	begin
		if Generator.Id_Property = null and then Is_Id( Property.all ) then
			Generator.Id_Property := Property;
		end if;

		case Val.Type_Of is
				when APQ_Smallint =>
					Spec_Smallint(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Integer =>
					Spec_Integer(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Bigint =>
					Spec_Bigint(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Real =>
					Spec_Real(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Double =>
					Spec_Double(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Serial =>
					Spec_Serial(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Bigserial =>
					Spec_Bigserial(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);


				when APQ_Date =>
					Spec_Date(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Time =>
					Spec_Time(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when APQ_Timestamp =>
					Spec_Timestamp(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);


				when Hour_Number =>
					Spec_Hour(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when Minute_Number =>
					Spec_Minute(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);

				when Second_Number =>
					Spec_Second(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);


				when APQ_String =>
					Spec_String(
							Generator	=> Create_Generator_Type'Class( Generator ),
							Property	=> Property.all,
							Q		=> Q
						);
		end case;
	end Append_Column_Specification;



	----------------------
	-- ID Specification --
	----------------------
	procedure Append_Id_Specification(
			Generator	: in out Create_Generator_Type;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		-- append the index id specification
	begin
		APQ.Append( Q, "PRIMARY KEY(" & Generator.Id_Property.Name.all & ")" );
	end Append_Id_Specification;


	-----------------------------------------
	-- Column Specification Helper Methods --
	-----------------------------------------


	-- numeric values
	procedure Spec_Smallint(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "SMALlINT",
				Q		=> Q
			);
	end ;

	procedure Spec_Integer(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "INTEGER",
				Q		=> Q
			);
	end ;

	procedure Spec_Bigint(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "BIGINT",
				Q		=> Q
			);
	end ;

	procedure Spec_Real(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "REAL",
				Q		=> Q
			);
	end ;

	procedure Spec_Double(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "DOUBLE",
				Q		=> Q
			);
	end ;

	procedure Spec_Serial(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		function Get_Type_Spec return String is
			use APQ;
		begin
			if Engine_of( Q ) = Engine_PostgreSQL then
				return "SERIAL";
			else
				return "INT";
			end if;
		end Get_Type_Spec;
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Get_Type_Spec,
				Q		=> Q
			);
	end ;

	procedure Spec_Bigserial(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		function Get_Type_Spec return String is
			use APQ;
		begin
			if Engine_of( Q ) = Engine_PostgreSQL then
				return "BIGSERIAL";
			else
				return "BIGINT";
			end if;
		end Get_Type_Spec;
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Get_Type_Spec,
				Q		=> Q
			);
	end ;



	-- date/time/timestamp

	procedure Spec_Date(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "DATE",
				Q		=> Q
			);
	end ;

	procedure Spec_Time(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "TIME",
				Q		=> Q
			);
	end ;

	procedure Spec_Timestamp(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> "DATETIME",
				Q		=> Q
			);
	end ;


	-- hour/minute/second

	function Time_Number_Spec( Q : in APQ.Root_Query_Type'Class ) return String is
		-- this function is used for all time data types
		use APQ;
	begin
		case Engine_Of( Q ) is
			when Engine_PostgreSQL | Engine_Sybase | Engine_CT_Lib =>
				return "TINYINT";
			when others =>
				return "SMALLINT";
		end case;
	end Time_Number_Spec;

	procedure Spec_Hour(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Time_Number_Spec( Q ),
				Q		=> Q
			);
	end ;

	procedure Spec_Minute(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Time_Number_Spec( Q ),
				Q		=> Q
			);
	end ;

	procedure Spec_Second(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Time_Number_Spec( Q ),
				Q		=> Q
			);
	end ;



	-- string

	procedure Spec_String(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		function Get_Type_Spec return String is
			Value : Value_Type := Get_Value( Property );
		begin
			if Value.String_Length <= 255 then
				-- even though since mysql 5.0 the varchar can store bigger strings
				-- I'm limiting this to 255 characters for compatibility reasons
				--
				-- and I am assuming all the database vendors support at least 255 charaters in VARCHAR elements.
				return "VARCHAR (" & Natural'Image( Value.String_Length ) & ')';
			else
				-- and I'll support TEXT at most; if the user wants to use a bigger string
				-- it's probably better to store it in a text file and then load it using
				-- streams.
				--
				-- NOTE: it might be a good idea to implement stream types in KOW_Ent, but it's going to be a huge task
				return "TEXT";
			end if;
		end Get_Type_Spec;
	begin
		General_Spec(
				Generator	=> Create_Generator_Type'Class( Generator ),
				Property	=> Property,
				Type_Spec	=> Get_Type_Spec,
				Q		=> Q
			);
	end ;



	---------------------------
	-- Auxiliar Spec Methods --
	---------------------------


	procedure General_Spec(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Type_Spec	: in     String;
			Q		: in out APQ.Root_Query_Type'Class
		) is
	begin
		APQ.Append( Q, Property.name.all & " " & Type_Spec );
		if not Property.Allow_Null then
			APQ.Append( Q, " NOT NULL" );
		end if;

		if Is_Id( Property ) and then Get_Value( Property ).Type_Of in APQ_Serial .. APQ_Bigserial then
			Spec_Auto_Increment(
					Generator	=> Create_Generator_Type'Class( Generator ),
					Property	=> Property,
					Q		=> Q
				);
		end if;
	end General_Spec;

	procedure Spec_Auto_Increment(
			Generator	: in out Create_Generator_Type;
			Property	: in     KOW_Ent.Property_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is
		use APQ;
		Type_Of : constant Type_Of_Data_Type := Get_Value( Property ).Type_Of;
	begin
		pragma Assert( Type_Of in APQ_Serial .. APQ_Bigserial, "Only APQ_Serial and APQ_Bigserial types can be auto-incrementing!" );

		case APQ.Engine_Of ( Q ) is
			when Engine_PostgreSQL =>
				-- serial type in PostgreSQL is already auto-increment
				null;

			when Engine_MySQL =>
				APQ.Append( Q, " AUTO_INCREMENT" );

			when Engine_Sybase | Engine_CT_Lib =>
				-- this is the syntax for Ms SQL Server.
				-- it should be valid for Sybase as well (at least it's the same keyword)
				APQ.Append( Q, " identity(1,1)" ); 

			when others =>
				raise PROGRAM_ERROR with "Tryed to run KOW_Ent in a non-supported database backend driver. Sorry about that, but you'll have to create your own db schema. Everything else should work fine.";
		end case;
	end Spec_Auto_Increment;

end KOW_Ent.SQL.Create;


