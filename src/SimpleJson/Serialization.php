<?php

// converts an object tree to another that
// can be converted to json
function convertToJson($obj) {
    if (is_null($obj)) {
        return NULL;
    }
    else if ($obj instanceof FSharpList)
    {
        $array = [];
        foreach($obj as $value)
        {
            $array[] = convertToJson($value);
        }

        return [ '_list' => $array];

    }
    else if ($obj instanceof Union)
    {
        $props = [];
        foreach(get_object_vars($obj) as $prop => $value)
        {
            $props[] = convertToJson($value);
        }
        return ['_case' => $obj->get_Case(), 'fields' => $props ];
    }
    else if (is_array($obj))
    {
        $array = [];
        foreach($obj as $key => $value)
        {
            $array[$key] = convertToJson($value);
        }
        return $array;
    }
    else if (is_object($obj))
    {
        $props = [];
        $props['_type'] = get_class($obj);
        foreach(get_object_vars($obj) as $prop => $value)
        {
            $props[$prop] = convertToJson($value);
        }
        return $props;
    }
    else
        return $obj;
} 


// converts a json parsed object tree
// to an actual F# object tree
function convertFromJson($json) {
    if (is_null($json))
    {
        return NULL;
    }
    if (is_object($json))
    {
        if (property_exists($json, '_case'))
        {
            $case = $json->_case;

            $args=[];
            foreach($json->fields as $value)
            {
                $args[] = convertFromJson($value);
            }


            return new $case(...$args);
        }
        if (property_exists($json, '_type'))
        {
            $type = $json->_type;
            $args=[];
            foreach(get_object_vars($json) as $prop => $value)
            {
                if ($prop != '_type')
                {
                    $args[] = convertFromJson($value);
                }
            }
            return new $type(...$args);
        }
        if (property_exists($json, '_list'))
        {
            $array = [];
            foreach($json->_list as $value)
            {
                $array[] = convertFromJson($value);
            }
            return FSharpList::ofArray($array);
        }
        
        $props = [];
        foreach(get_object_vars($json) as $prop => $value)
        {
            $props[$prop] = convertFromJson($value);
        }

        return (object)$props;
    }
    if (is_array($json))
    {
            $array = [];
            foreach($json as $value)
            {
                $array[] = convertFromJson($value);
            }
            return $array;
    }
    return $json;

}



// converts an object tree to another that
// can be converted to json
function convertToSimpleJson($obj) {
    if (is_null($obj)) {
        return NULL;
    }
    else if ($obj instanceof FSharpList)
    {
        $array = [];
        foreach($obj as $value)
        {
            $array[] = convertToSimpleJson($value);
        }

        return $array;

    }
    else if ($obj instanceof FSharpUnion)
    {
        $vars = get_object_vars($obj);
        if (empty($vars))
            return $obj->get_FSharpCase();

        $props = [$obj->get_FSharpCase()];
        foreach($vars as $prop => $value)
        {
            $props[] = convertToSimpleJson($value);
        }
        return $props;
    }
    else if (is_array($obj))
    {
        $array = [];
        foreach($obj as $key => $value)
        {
            $array[$key] = convertToSimpleJson($value);
        }
        return $array;
    }
    else if (is_object($obj))
    {
        $props = [];
        foreach(get_object_vars($obj) as $prop => $value)
        {
            $props[$prop] = convertToSimpleJson($value);
        }
        return $props;
    }
    else
        return $obj;
} 

function convertType($json, $cls)
{
    $class_vars = get_class_vars($cls);
    if (count($class_vars) == 0) {
        return new $cls();
    } elseif (count($class_vars) == 1) {
        $fieldName =key($class_vars);
        $method = "get_{$fieldName}_Type";
        $field = convertFromSimpleJson($json, $cls::$method());
        return new $cls($field);
    } else {
        $fields = [];
        foreach ($class_vars as $field => $_) {
            $method = "get_{$field}_Type";
            $fields[$field] = convertFromSimpleJson($json->$field, $cls::$method());
        }
        return new $cls(...$fields);
    }
}

function convertFromSimpleJson($json, $cls)
{
    if (is_null($json)) {
        return null;
    }

    if ($cls == 'String') {
        return $json;
    }
    if ($cls == 'Int32') {
        return $json;
    }

    if (is_array($cls)) {
        if ($cls[0] == "List") {
            $result = [];
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $cls[1]);
            }
            return FSharpList::ofArray($result);
        } elseif ($cls[0] == "Set") {
            $result = [];
            $itemCls = $cls[1];
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $itemCls);
            }
            return Set::ofSeq($result, ['Compare' => 'Util::compare']);
        } elseif ($cls[0] == "Map") {
            $result = [];
            $keyCls = $cls[1];
            $valueCls = $cls[2];
            foreach ($json as $item) {
                $result[] = [
                    convertFromSimpleJson($item[0], $keyCls),
                    convertFromSimpleJson($item[1], $valueCls)
                ];
            }
            return Map::ofArray($result);
        } elseif ($cls[0] == "Tuple") {
            $result = [];
            $index = 1;
            foreach ($json as $item) {
                $result[] = convertFromSimpleJson($item, $cls[$index]);
                $index++;
            }
            return $result;
        }
    }

    if (is_subclass_of($cls, "FSharpUnion")) {
        if (is_string($json)) {
            foreach ($cls::allCases() as $case) {
                if ($case::get_FSharpCase() == $json) {
                    return new $case();
                }
            }
        } elseif (is_object($json)) {
            foreach ($cls::allCases() as $caseCls) {
                $caseName = $caseCls::get_FSharpCase();
                if (property_exists($json, $caseName)) {
                    return convertType($json->$caseName, $caseCls);
                }
            }
        }
    }

    // Record (should be at least iComparable)
    if (is_subclass_of($cls, "iComparable")) {
        return convertType($json, $cls);
    }

    throw new Error("Not sure how to decode this");
}
