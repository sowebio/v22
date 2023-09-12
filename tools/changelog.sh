#!/bin/bash

changelog=$(git log --oneline)

echo "procedure On_Changelog (Object : in out Base.Base_Type'Class) is"
echo "begin"
echo "   Framework.Header_Notify_Menu_Click (Object, \"Changelog\");"
echo ""
echo "   Framework.Content_Set_Title (Object, \"Changelog\");"
echo "   Framework.Content_Set_Text (Object, \"\");"
echo "end On_Changelog;"

function create_procedure () {
	ID=$(echo $line | cut -f1 -d" ")
	content=$(echo $line | cut -d" " -f 2-)
	title="Changelog $ID"
	name="Changelog_$ID"
	funcname="On_Changelog_$ID"

	echo "procedure $funcname (Object : in out Base.Base_Type'Class) is"
	echo "begin"
	echo "   Framework.Header_Notify_Menu_Click (Object, \"$name\");"
	echo ""
	echo "   Framework.Content_Set_Title (Object, \"$title\");"
	echo "   Framework.Content_Set_Text (Object, \"$content\");"
	echo "end $funcname;"
}

function setup_tree () {
	ID=$(echo $line | cut -f1 -d" ")
	content=$(echo $line | cut -d" " -f 2-)
	title="Changelog $ID"
	name="Changelog_$ID"
	funcname="On_Changelog_$ID"

	echo "Framework.Header_Add_Child (\"$name\", \"$title\", \"Changelog\", $funcname'Unrestricted_Access);"
}

git log --oneline | while read line ; do
	create_procedure $line
	echo ""
done

echo "#################"
echo "Next needs to be placed below Framework.Setup"
echo "#################"

echo "Framework.Header_Add_Child (\"Changelog\", \"Changelog\", \"App_Menu\", On_Changelog'Unrestricted_Access);"

git log --oneline | while read line ; do
	setup_tree $line
done

