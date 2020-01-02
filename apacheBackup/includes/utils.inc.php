<?php
if(!defined('BASE_URI')){
    require_once('./config.inc.php');
    trigger_error("unauthorized access");
    exit();
}

// Send messages to elm app as Json 
function print_json_message($msg){
    $result = array('message' => $msg);
    echo (json_encode($result));
}

function print_json_error($msg){
    $result = array('serverError' => $msg);
    echo (json_encode($result));
}

function get_json_post_data(){
    $json_data = file_get_contents("php://input");
    $php_data = json_decode($json_data);

    if (is_null($php_data)){
        trigger_error("json data could not be decoded");
        logError("json data could not be decoded");
        exit();
    }

    return $php_data;   
}