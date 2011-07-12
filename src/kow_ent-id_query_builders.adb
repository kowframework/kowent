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


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Properties;

---------
-- APQ --
---------
with APQ;
with APQ_Provider;

package body KOW_Ent.ID_Query_Builders is

	--------------------
	-- Helper Methods --
	--------------------

	procedure Append_Unique(
			V	: in out KOW_Lib.Ustring_Vectors.Vector;
			Value	: in     Unbounded_String
		) is
		use KOW_Lib.UString_Vectors;
	begin
		if not Contains( V, Value )  then
			Append( V, Value );
		end if;
	end Append_Unique;


	procedure Merge_Unique(
			Container	: in out KOW_Lib.UString_Vectors.Vector;
			Source		: in     KOW_Lib.UString_Vectors.Vector
		) is

		procedure Appender( C : in KOW_lib.Ustring_Vectors.Cursor ) is
		begin
			Append_Unique( Container, KOW_Lib.UString_Vectors.Element( C ) );
		end Appender;
	begin
		KOW_Lib.UString_Vectors.Iterate( Source, Appender'Access );
	end Merge_Unique;



	function To_List(
			Entity_Tags	: in KOW_Lib.UString_Vectors.Vector;
			Ignore_tag	: in Unbounded_String
		) return Entity_Information_Lists.List is
	
		Result : Entity_Information_Lists.List;

		procedure Appender( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Tag : constant Unbounded_String := KOW_Lib.UString_Vectors.Element( C );
		begin
			if Tag = "" or else Tag = Ignore_Tag then
				return;
			else
				Entity_Information_Lists.Append( Result, KOW_Ent.Entity_Registry.Get_Information( Tag ) );
			end if;
		end Appender;
			
	begin
		KOW_Lib.Ustring_Vectors.Iterate( Entity_Tags, Appender'Access );
		return Result;
	end To_List;



	-----------------------------------------------------------------------------------------------------------------

	----------------------
	-- Query Management --
	----------------------

	function To_String( Q : in Query_Type ) return String is
		-- return the query as string
		Buffer : Unbounded_String;
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			Build_Query( Q, Query, Connection );
			Buffer := To_Unbounded_String( APQ.To_String( Query ) );
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );

		return To_String( Buffer );
	end To_String;




	procedure Clear( Q : in out Query_Type ) is
		-- clear the query for reuse
	begin
		Operator_Vectors.Clear( Q.Operators );
		Order_By_Vectors.Clear( Q.Order_By );
	end Clear;


	procedure Prepare( Q : in out Query_Type; Entity_Tag : in Ada.Tags.Tag ) is
		-- clear the query and set it up for the given entity tag
	begin
		Prepare(
				Q		=> Q,
				Entity_Tag	=> To_Unbounded_String(
							Ada.Tags.Expanded_Name( Entity_Tag )
						)
			);
	end Prepare;

	procedure Prepare( Q : in out Query_Type; Entity_Tag : in Unbounded_String ) is
		-- clear the query and set it up for the given entity tag
	begin
		Clear( Q );
		Q.The_Entity_Tag := Entity_Tag;
	end Prepare;


	function Entity_Tag( Q : in Query_Type ) return Unbounded_String is
	begin
		pragma Assert( Q.The_Entity_Tag /= Null_Unbounded_String, "you haven't set the entity tag" );
		return Q.The_Entity_Tag;
	end Entity_Tag;



	--
	-- Foreign Key
	--

	procedure Append(
				Q		: in out Query_Type;
				Foreign_Key	: in     KOW_Ent.Entity_Type'Class;
				Appender	: in     Logic_Appender := Appender_AND;
				Operator	: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element based on foreign key
		Properties : KOW_Ent.Property_Lists.List;
		Found_Foreign_Key : Boolean := False;

		-- TODO: create an index for foreign key properties in the entity registry

		procedure Iterator( C : in KOW_Ent.Property_Lists.Cursor ) is
			use KOW_Ent.Properties;
			E : KOW_Ent.Entity_Property_Ptr;
		begin
			if Found_Foreign_Key then
				return;
			end if;

			E := KOW_Ent.Property_Lists.Element( C );
			if E.all in Foreign_Key_Property_Type then
				if Foreign_key_Property_Type( E.all ).Related_Entity_Tag = Foreign_Key'Tag then
					Found_Foreign_Key := True;
					Append(
						Q	=> Q,
						Column	=> E.Column_Name,
						Value	=> Foreign_Key.ID,
						Appender=> Appender,
						Operator=> Operator
					);
				end if;
			end if;
		end Iterator;
				
	begin

		-- first we gotta get all the properties for this entity
		Properties := KOW_Ent.Entity_Registry.Get_Properties( Entity_Tag( Query_Type'Class( Q ) ) );

		-- and now we try to find out which one is a foreign key
		KOW_Ent.Property_Lists.Iterate( Properties, Iterator'Access );
	end Append;


	-- 
	-- KOW_Ent.Id_Type

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
	begin
		Append(
			Q		=> Q,
			Column		=> To_Unbounded_String( Column ),
			Value		=> Value,
			Appender	=> Appender,
			Operator	=> Operator
		);
	end Append;

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
	begin
		Append(
			Q		=> Q,
			Column		=> Column,
			Value		=> To_Unbounded_String( To_String( Value ) ),
			Appender	=> Appender,
			Operator	=> Operator
		);
	end Append;


	--
	-- String 
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> To_Unbounded_String( Column ),
			Value	=> To_Unbounded_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append;



	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> To_Unbounded_String( Column ),
			Value	=> Value,
			Appender=> Appender,
			Operator=> Operator
		);
	end Append;


	
	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
		Handler : Operator_Handler_Type( Data_Type => Is_String );
	begin
		Handler.Column		:= Column;
		Handler.String_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );
	end Append;




	-- 
	-- Password
	--
	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
	begin
		Append(
			Q	=> Q,
			Column	=> Column,
			Value	=> KOW_Ent.Calculate_Hash( Value ),
			Appender=> Appender,
			Operator=> Operator
		);

	end Append_Password;



	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
	begin
		Append_Password(
			Q	=> Q,
			Column	=> Column,
			Value	=> To_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append_Password;

	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		-- append a query element to this query
	begin
		Append_Password(
			Q	=> Q,
			Column	=> To_String( Column ),
			Value	=> To_String( Value ),
			Appender=> Appender,
			Operator=> Operator
		);
	end Append_Password;



	--
	-- Percent 
	-- 

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Percent;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Percent );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Percent_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );

	end Append;


	--
	-- Money
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Money;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Money );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Money_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );

	end Append;



	--
	-- Date 
	--

	procedure Append_Date(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Date;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Date );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Date_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );

	end Append_Date;



	--
	-- Timestamp 
	--

	procedure Append_Timestamp(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Timestamp;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Timestamp );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Timestamp_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;

		Operator_Vectors.Append( Q.Operators, Handler );

	end Append_Timestamp;


	--
	-- Dimension 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Dimension;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Dimension );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Dimension_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;


		Operator_Vectors.Append( Q.Operators, Handler );

	end Append;


	--
	-- Weight 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Weight;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Weight );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Weight_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;


		Operator_Vectors.Append( Q.Operators, Handler );
	end Append;




	--
	-- Count 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     KOW_Ent.Extra_Properties.Count;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			) is
		Handler : Operator_Handler_Type( Data_Type => Is_Count );
	begin
		Handler.Column		:= To_Unbounded_String( Column );
		Handler.Count_Value	:= Value;
		Handler.Appender	:= Appender;
		Handler.Operator	:= Operator;
		Handler.Operation_Type	:= L_Operator;


		Operator_Vectors.Append( Q.Operators, Handler );

	end Append;





	--
	-- Sub Queries
	--




	procedure Append(
				Q	: in out Query_Type;
				Child_Q	: in     Query_Type;
				Appender: in     Logic_Appender := Appender_AND
			) is
		-- append another query as a child query :: () stuff
		Handler : Operator_Handler_Type( Data_Type => Is_None );
	begin
		Handler.Child_Query.Q := new Query_Type'( Child_Q );
		Handler.Appender := Appender;
		Handler.Operation_Type := Q_Operator;


		if Entity_Tag( Q ) /= Entity_Tag( Child_Q ) then
			Append_Unique( Q.Related_Entity_Tags, Entity_Tag( Child_Q ) );
		end if;

		Operator_Vectors.Append( Q.Operators, Handler );
	end Append;


	--------------
	-- Order By --
	--------------

	procedure Append_Order(
				Q		: in out Query_Type;
				Column		: in     Unbounded_String;
				Ordenation	: in     Ordenation_Type := ASCENDING
		 	) is
	begin
		Order_By_Vectors.Append(
				Q.Order_By,
				(
					Column		=> Column,
					Ordenation	=> Ordenation
				)
			);
	end Append_Order;


	procedure Append_Order(
				Q		: in out Query_Type;
				Column		: in     String;
				Ordenation	: in     Ordenation_Type := ASCENDING
		 	) is
	begin
		Append_Order(
				Q		=> Q,
				Column		=> To_Unbounded_String( Column ),
				Ordenation	=> Ordenation
			);
	end Append_Order;




	-----------------------------------------------------------------------------------------------------------------


	-----------------------------
	-- Generic Implementations --
	-----------------------------


	-- 
	-- All
	--
	procedure Generic_All_Iterator( Q : in Query_Type ) is
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );

		begin
			Build_Query( Q, Query, Connection );
			APQ.Execute( Query, Connection );
			begin
				loop
					APQ.Fetch( Query );
					Iterator( Query, Connection );
				end loop;
			exception
				when APQ.No_Tuple =>
					null;
			end;
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );
	end Generic_All_Iterator;


	-- 
	-- Some
	--

	
	procedure Generic_Some_Iterator(
				Q	: in Query_Type;
				From	: in Positive;
				Limit	: in Natural
			) is
			use ada.text_io;
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			use APQ;
		begin
			Build_Query( Q, Query, Connection, From, Limit );
			APQ.Execute( Query, Connection );

			if APQ.Engine_Of( Query ) = Engine_MySQL then
				begin
					loop
						APQ.Fetch( Query );
						Iterator( Query, Connection );
					end loop;
				exception
					when APQ.No_Tuple =>
						null;
				end;
			else
				begin
					for i in 1 .. Natural( From ) - 1 loop
						APQ.Fetch( Query );
					end loop;

					if Limit /= 0 then
						for i in From .. From + Positive( Limit ) loop
							APQ.Fetch( Query );
							Iterator( Query, Connection );
						end loop;

						loop
							-- fetch all other results just so we don't trash the connection..
							-- NOTE: MAYBE, just MAYBE trashing the connection isn't that bad
							-- as APQ_Provider will manage to reconnect the next time it is needed..
							APQ.Fetch( Query );
						end loop;
					else
						loop
							APQ.Fetch( Query );
							Iterator( Query, COnnection );
						end loop;
					end if;

				exception
					when others => null;
				end;

			end if;
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );
	end Generic_Some_Iterator;

	--
	-- First 
	--
	procedure Generic_First_Iterator(
				Q	: in Query_Type;
				Unique	: in Boolean
			) is
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			Build_Query( Q, Query, Connection, 1, 2 );
			APQ.Execute( Query, Connection );
			APQ.Fetch( Query );
			Iterator( Query, Connection );
			begin
				loop
					APQ.Fetch( Query );
					-- if it got here, then check if it's ok to have duplicated results:
					if Unique then
						raise DUPLICATED_ENTITY_ELEMENT with "Tag :: " & Ada.Tags.Expanded_Name( Entity_Type'Tag );
					end if;
				end loop;
			exception
				when APQ.No_Tuple => null;
			end;
		end Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Runner'Access );
	end Generic_First_Iterator;


	---------------------
	-- Retrieving Data --
	---------------------


	--
	-- All
	--

	function Get_All( Q : in Query_Type ) return KOW_Ent.ID_Array_Type is
		-- get all results from the query
		-- if no results, raise NO_ENTITY

		Tons_OF_Ids	: KOW_Ent.ID_Array_Type( 1 .. 10000 );
		Last_ID		: Natural := Natural'First;

		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
		begin
			Last_ID := Last_ID + 1;
			Tons_Of_IDs( Last_ID ) := KOW_Ent.TO_ID( Integer( KOW_Ent.ID_Value( Query, APQ.Column_Index( Query, "id" ) ) ) );
		end Set_Value;

		procedure Iterate is new Generic_All_Iterator( Set_Value );
	begin
		Iterate( Q );
		return Tons_of_IDs( 1 .. Last_ID );
	exception
		when e : CONSTRAINT_ERROR =>
			if Last_ID > Tons_of_IDs'Last then
				return Tons_of_IDs;
			else
				Ada.Exceptions.Reraise_Occurrence( e );
			end if;
	end Get_All;



	--
	-- Some
	--

	function Get_Some(
				Q		: in     Query_Type;
				From		: in     Positive;
				Limit		: in     Natural
			) return KOW_Ent.Id_Array_Type is
		-- if limit = 0, get all results
		
		function max_results return positive is
		begin
			if limit = 0 then
				return 1_000;
			else
				return positive( limit );
			end if;
		end max_results;

		Results : KOW_Ent.Id_Array_type( 1 .. max_results );
		Total	: Natural := 0;

		
		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
		begin
			Total := Total + 1;
			Results( Total ) := KOW_Ent.TO_ID( Integer( KOW_Ent.ID_Value( Query, APQ.Column_Index( Query, "id" ) ) ) );
		end Set_Value;

		procedure Iterate is new Generic_Some_Iterator( Iterator => Set_Value );
	begin
		Iterate( Q, From, Limit );

		if Total = 0 then
			return Results( 2 .. 1 ); -- empty array
		else
			return Results( 1 .. Positive( Total ) );
		end if;
	end Get_Some;



	--
	-- First
	--


	function Get_First( Q : in Query_Type; Unique : in Boolean ) return KOW_Ent.ID_Type is
		-- get the first element from the query
		-- if no results, raise NO_ENTITY
		-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.


		Result : KOW_Ent.ID_Type;

		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
		begin
			Result := KOW_Ent.To_ID( Integer( KOW_Ent.ID_Value( Query, APQ.Column_Index( Query, "id" ) ) ) );
		end Set_Value;


		procedure Iterate is new Generic_First_Iterator( Set_Value );
	begin
		Iterate( Q, Unique );

		return Result;
	exception
		when APQ.No_Tuple =>
			raise NO_ENTITY with "Tag :: " & Ada.Tags.Expanded_Name( Entity_Type'Tag );

	end Get_First;



	--
	-- Count
	--

	function Count( Q : in Query_Type ) return Natural is
		Total_Count : Natural;
		function Natural_Value is new APQ.Integer_Value( Val_Type => Natural );
		
		procedure Count_Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			use APQ;
			Count_Query	: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );

		begin
			Build_Query(
					Q		=> Q, 
					Query		=> Count_Query,
					Connection	=> Connection,
					Count_Query	=> True
				);

			APQ.Execute( Count_Query, Connection );
			APQ.Fetch( Count_Query );
			Total_Count := Natural_Value( Count_Query, APQ.Column_Index( Count_Query, "rowscount" ) );
			-- the coult query aways return only 1 result, so we are all good
		end Count_Runner;
	begin
		APQ_Provider.Run( KOW_Ent.My_Provider.all, Count_Runner'Access );

		return Total_Count;
	end Count;





	------------------------
	-- SQL Query Building --
	------------------------

	procedure Build_Query(
				Q		: in      Query_Type;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				Count_Query	: in     Boolean := False
			) is
		use Ada.Containers;	-- = operator for Count type
		Info			: Entity_Information_Type	:= Entity_Registry.Get_Information( Entity_Tag( Query_Type'Class( Q ) ) );
		Related_Entity_Tags	: Entity_Information_Lists.List := To_List( Get_Related_Entity_Tags( Q ), Entity_Tag( Q ) );


		Table_Name		: constant String := To_String( Info.Table_Name );

		procedure Append_Select( Field : in String; Not_First : Boolean := True ) is
		begin
			if Not_First then
				APQ.Append( Query, "," );
			end if;
			APQ.Append( Query, Table_Name & '.' & Field & " as " & Field );
		end Append_Select;

		procedure Append_Conditions( Append_Where : in Boolean ) is
		begin
			if Append_Where then
				APQ.Append( Query, " WHERE " );
				Append_To_APQ_Query( Q, Query, Connection );
			else
				APQ.Append( Query, " AND " );
				Append_To_APQ_Query( Q, Query, Connection );
				APQ.Append( Query, ")" );
			end if;
		end Append_Conditions;


		procedure Append_Order is
		begin
			APQ.Append( Query, " " );
			Append_Order_By_to_APQ_Query( Q, Query, Connection );
		end Append_Order;



		procedure Append_Table_Names( C : in Entity_Information_Lists.Cursor ) is
		begin
			APQ.Append( Query, "," & To_String( Entity_Information_Lists.Element( C ).Table_Name ) );
		end Append_Table_Names;


		First : Boolean := True;
		procedure Append_Relation_Clauses( C : in Entity_Information_Lists.Cursor ) is
			use Ada.Tags;
		
			Related_Info : Entity_Information_Type := Entity_Information_Lists.Element( C );


			function Get_Property return KOW_Ent.Properties.Foreign_Key_Property_Type is
				Prop	: KOW_Ent.Properties.Foreign_Key_Property_Type;
				Found	: Boolean := False;
				Self	: Boolean := False;

				procedure Prop_Iterator( C : in Property_Lists.Cursor ) is
					P : constant Entity_Property_Ptr := Property_lists.Element( C );
				begin
					if not Found and then P /= null and then P.all in KOW_Ent.Properties.Foreign_Key_Property_Type'Class then
						Prop := KOW_Ent.Properties.Foreign_Key_Property_Type( P.all );

						if Self then 
							if Prop.Related_Entity_Tag = Related_Info.Entity_Tag then
								Found := True;
							end if;
						else
							if Prop.Related_Entity_Tag = Info.Entity_Tag then
								Found := True;
							end if;
						end if;
					end if;
				end Prop_Iterator;
			begin
				Self := True;
				Property_Lists.Iterate( Info.Properties, Prop_Iterator'Access );
				if Found then
					return Prop;
				end if;


				Self := False;
				Property_Lists.Iterate( Related_Info.Properties, Prop_iterator'Access );
				if Found then
					return Prop;
				end if;

				raise CONSTRAINT_ERROR with "Not a related entity couple: " & Expanded_name( Info.Entity_Tag ) & " :: " & Expanded_name( Related_info.Entity_Tag );
			end Get_Property;

			The_Property : constant KOW_Ent.Properties.Foreign_Key_Property_Type := Get_Property;
		begin
			
			if First then
				First := False;
			else
				APQ.Append( Query, " AND " );
			end if;

			if The_Property.Related_Entity_Tag = Info.Entity_Tag then
				APQ.Append( Query, To_String( Info.Table_Name ) & ".id=" );
				APQ.Append( Query, To_String( Related_Info.Table_Name ) & "." & To_String( The_Property.Column_Name ) );
			elsif The_Property.Related_Entity_Tag = Related_Info.Entity_Tag then
				APQ.Append( Query, To_String( Related_Info.Table_Name ) & ".id=" );
				APQ.Append( Query, To_String( Info.Table_Name ) & "." & To_String( The_Property.Column_Name ) );
			else
				raise PROGRAM_ERROR with "you found a bug in KOW_Ent... Please report";
			end if;

		end Append_Relation_Clauses;

	begin
		if Count_Query then
			APQ.Prepare( Query, "SELECT count(*) as rowscount " );
		else
			APQ.Prepare( Query, "SELECT id,original_tag,filter_tags" );
			Append_Select( "id", False );
			Append_Select( "original_tag" );
			Append_Select( "filter_Tags" );

			Append_Column_Names_For_Read( Query, Info, "," );
		end if;
		APQ.Append( Query, " FROM " & To_String( Info.Table_Name )  );


		
		Entity_Information_Lists.Iterate( Related_Entity_Tags, Append_Table_Names'Access );


		if Entity_Information_Lists.Length( Related_Entity_Tags ) = 0 then
			Append_Conditions( Append_Where => true );
			Append_Order;
		else
			APQ.Append( Query, " WHERE " );

			Entity_Information_Lists.Iterate( Related_Entity_Tags, Append_Relation_Clauses'Access );

			Append_Conditions( Append_Where => false );

			APQ.Append( Query, " GROUP BY " & Table_Name & ".id" );

			Append_Order;
		end if;

	end Build_Query;


	procedure Build_Query(
				Q		: in     Query_Type;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				From		: in     Positive;
				Limit		: in     Natural
			) is
		use APQ;
	begin
		Build_Query( Q, Query, Connection );

		if Engine_of( Connection ) = Engine_MySQL then
			Append( Query, " LIMIT " & Natural'Image( Natural( From ) - 1 ) );
			if Limit /= 0 then
				Append( Query, "," & Natural'Image( Limit ) );
			end if;
		end if;
	end Build_Query;




	procedure Append_to_APQ_Query(
				Q		: in     Query_Type;
				APQ_Q		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			) is
		First_Element : Boolean := True;

		
		procedure Append_Appender( Appender : Logic_Appender ) is
			-- Append the appender.
			-- Use while appending child queries AND operators
		begin
			if First_Element then
				First_Element := False;
				APQ.Append( APQ_Q, " " );
			else
				case Appender is
					when Appender_AND =>
						APQ.Append( APQ_Q, " AND " );
					when Appender_OR  =>
						APQ.Append( APQ_Q, " OR " );
				end case;
			end if;
		end Append_Appender;

		procedure Append_Operator( C : Operator_Vectors.Cursor ) is
			-- an iterator for the operators
			Handler : Operator_Handler_Type := Operator_Vectors.Element( C );
		begin
			Append_Appender( Handler.Appender );
			case Handler.Operation_Type is
				when L_Operator =>
					APQ.Append( APQ_Q, To_String( Handler.Column ) );
					case Handler.Operator is
						when Operator_Equal_To =>
							APQ.Append( APQ_Q, "=" );
						when Operator_Not_Equal_To =>
							APQ.Append( APQ_Q, "!=" );
						when Operator_Like =>
							APQ.Append( APQ_Q, " LIKE " );
						when Operator_Less_Than =>
							APQ.Append( APQ_Q, " < " );
						when Operator_Less_Than_Or_Equal_To =>
							APQ.Append( APQ_Q, " <= " );
						when Operator_Greater_Than =>
							APQ.Append( APQ_Q, " > " );
						when Operator_Greater_Than_Or_Equal_To =>
							APQ.Append( APQ_Q, " >= " );

					end case;


					case Handler.Data_Type is
						when Is_String => 
							APQ.Append_Quoted( 
									APQ_Q, 
									Connection, 
									To_String( Handler.String_Value )
								);

						when Is_Percent =>
							KOW_Ent.Extra_Properties.Percent_Properties.Append( APQ_Q, Handler.Percent_Value );
						when Is_Money =>
							KOW_Ent.Extra_Properties.Money_Properties.Append( APQ_Q, Handler.Money_Value );
						when Is_Date =>
							KOW_Ent.Extra_Properties.Date_Properties.Append( APQ_Q, Handler.Date_Value );
						when Is_Timestamp =>
							KOW_Ent.Extra_Properties.Timestamp_Properties.Append( APQ_Q, Handler.Timestamp_Value );
						when Is_Dimension =>
							KOW_Ent.Extra_Properties.Dimension_Properties.Append( APQ_Q, Handler.Dimension_Value );
						when Is_Weight =>
							KOW_Ent.Extra_Properties.Weight_Properties.Append( APQ_Q, Handler.Weight_Value );
						when Is_Count =>
							KOW_Ent.Extra_Properties.Count_Properties.Append( APQ_Q, Handler.Count_Value );

						when Is_None =>
							raise CONSTRAINT_ERROR with "invalid logic operator in query";
					end case;


				when Q_Operator =>
					APQ.Append( APQ_Q, "(" );
					Append_to_APQ_Query( Handler.Child_Query.Q.all, APQ_Q, Connection );
					APQ.Append( APQ_Q, ")" );
			end case;
		end Append_Operator;
	begin
		Operator_Vectors.Iterate( Q.Operators, Append_Operator'Access );


		if First_Element then
			-- when it's hasn't processed any element..
			-- a dirty hack for child queries to work fine
			APQ.Append( APQ_Q, "1=1" );
		end if;
	end Append_to_APQ_Query;

	procedure Append_Order_by_to_APQ_Query(
				Q		: in     Query_Type;
				APQ_Q		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			) is
		Order_By_Str : Unbounded_String;

		First_Element : Boolean := true;
		procedure Iterator( C : in Order_By_Vectors.Cursor ) is
			O : Order_By_Type := Order_By_Vectors.Element( C );
		begin
			if First_Element then
				First_Element := False;
			else
				Order_by_Str := Order_by_Str & ',';
			end if;
			Order_By_Str := Order_By_Str & "ORDER BY " & O.Column;

			case O.Ordenation is
				when ASCENDING => Order_By_Str := Order_by_Str & " ASC";
				when DESCENDING => Order_By_Str := Order_by_Str & " DESC";
			end case;
		end Iterator;
	begin
		Order_By_Vectors.Iterate( Q.Order_By, Iterator'Access );
		if Order_by_Str /= "" then
			APQ.Append( APQ_Q, To_String( Order_By_Str ) );
		end if;
	end Append_Order_by_to_APQ_Query;


	function Get_Related_Entity_Tags(
				Q		: in     Query_Type
			) return KOW_Lib.UString_Vectors.Vector is
		-- query all the related entity tags from this and all the child queries
		Tags : KOW_Lib.UString_Vectors.Vector := Q.Related_Entity_Tags;


		procedure Append_Child_Query_Operator( C : Operator_Vectors.Cursor ) is
			-- an iterator for the operators
			Handler : Operator_Handler_Type := Operator_Vectors.Element( C );
		begin
			case Handler.Operation_Type is
				when Q_Operator	=>
					Merge_Unique(
							Container	=> Tags,
							Source		=> Get_Related_Entity_Tags( Handler.Child_Query.Q.all )
						);
				when others =>
					null;
			end case;
		end Append_Child_Query_Operator;
	begin
		Operator_Vectors.Iterate( Q.Operators, Append_Child_Query_Operator'Access );

		return Tags;
	end Get_Related_Entity_Tags;





-- private

	
	procedure Free is new Ada.Unchecked_Deallocation(
				Object	=> Query_Type,
				Name	=> Query_Ptr
			);

	overriding
	procedure Finalize( Container : in out Query_Container_Type ) is
	begin
		if Container.Q /= null then
			Free( Container.Q );
		end if;
	end Finalize;

	overriding
	procedure Adjust( Container : in out Query_Container_Type ) is
	begin
		if Container.Q /= null then
			Container.Q := new Query_Type'( Container.Q.all );
		end if;
	end Adjust;


end KOW_Ent.ID_Query_Builders;
