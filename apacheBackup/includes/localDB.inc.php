<?php # localDB.inc.php

if(!defined('BASE_URI')){
    require_once('./config.inc.php');
    trigger_error("unauthorized access");
    exit();
}
require_once('../includes/utils.inc.php');


$servername = 'localhost';
$username = 'testUser';
$password = '4NKIXUlOeXDuPueJ*';
$dbname = 'testDatabase';

try {
    $conn = new PDO("mysql:host=$servername;dbname=$dbname", $username, $password);
    // set the PDO error mode to exception
    $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
}
catch(PDOException $e){
    trigger_error("Database connection failed: " . $e->getMessage());
	print_json_error("something went wrong, we are working on it...");
	exit();
}
