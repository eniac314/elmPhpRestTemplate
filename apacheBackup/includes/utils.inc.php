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

function get_json_post_data($expected = array()){
    $json_data = file_get_contents("php://input");
    $post_data = json_decode($json_data, true);
    
    if (is_null($post_data)){
        trigger_error("json data could not be decoded");
        print_json_error("json data could not be decoded");
        exit();
    }

    if (!empty($expected)){
        foreach ($expected as $var) {
            if (!isset($post_data[$var])){
                trigger_error("$var has not been set");
                print_json_error("$var has not been set");
                exit();
            }
        }
    }

    return $post_data;   
}
