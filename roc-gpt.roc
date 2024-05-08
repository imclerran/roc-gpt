app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.9.0/JI4BuuOuWnD1R3Xcx-F8VrWdj-LM_FfDRB00ekYjIIQ.tar.br",
}

import cli.Http
import cli.Env
import cli.Task exposing [Task]
import cli.Stdout
import cli.Stdin
import json.Json

Message : { role : Str, content : Str }

ApiResponse : {
    id : Str,
    model : Str,
    object : Str,
    created : I64,
    choices : List {
        index : U8,
        message : Message,
        finishReason : Str,
    },
    usage : {
        promptTokens : U64,
        completionTokens : U64,
        totalTokens : U64,
        totalCost : F32,
    },
}

main =
    keyResult <- Task.attempt (Env.var "OPENROUTER_API_KEY")
    apiKey =
        when keyResult is
            Ok key -> key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"
    model = getModelChoice!
    Stdout.line! "Using model: $(model)\n"

    Task.loop! { model, apiKey, previousMessages: [systemMessage] } proompt
    Stdout.line "\nAssistant: Goodbye! ^_^"

systemMessage = {
    role: "system",
    content: "You are a helpful assistant, who answers questions in a concise and friendly manner. If you do not have knowledge about the on the users inquires about, you should politely tell them you cannot help.",
}


## The main loop for the chatbot
## This function prompts the user for input, sends the input to the OpenRouter API, and prints the response
## This will continue until the user types quit, exit, goodbye, or goodbye!
proompt = \{ model, apiKey, previousMessages } -> 
    Stdout.write! "You: "
    query = Stdin.line!
    when query |> strToLower is
        "quit" -> Task.ok (Done {apiKey, model, previousMessages})
        "exit" -> Task.ok (Done {apiKey, model, previousMessages})
        "goodbye" -> Task.ok (Done {apiKey, model, previousMessages})
        "goodbye!" -> Task.ok (Done {apiKey, model, previousMessages})
        _ ->
            messages = List.append previousMessages { role: "user", content: query }
            response = Http.send! (buildRequest apiKey model messages)
            
            when handleResponse messages response is
                Ok (Step updatedMessages) -> 
                    when List.last updatedMessages is
                        Ok { content } -> 
                            {} <- Stdout.write "\nAssistant: " |> Task.await # Compiler crash if using `!` syntax here
                            {} <- Stdout.line content |> Task.await # Compiler crash if using `!` syntax here
                            {} <- Stdout.write "\n" |> Task.await # Compiler crash if using `!` syntax here
                            Task.ok (Step {apiKey, model, previousMessages: updatedMessages})
                        Err _ -> Task.err EmptyMessageList
                Err _ -> Task.err ErrorHandlingResponse


## decode the response from the OpenRouter API and append the first message to the list of messages
handleResponse : List Message, Http.Response -> Result [Step (List Message)] _
handleResponse = \messages, response ->
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ApiResponse
    decoded = Decode.fromBytesPartial responseBody decoder
    when decoded.result is
        Ok body ->
            when List.get body.choices 0 is
                Ok choice -> 
                    updatedMessages = List.append messages choice.message
                    Ok (Step updatedMessages)
                Err _ -> Err "Error getting first choice"
        Err _ -> Err "Error decoding response"


## Build the request object for the OpenRouter API
buildRequest : Str, Str, List Message -> Http.Request
buildRequest = \apiKey, model, messages ->
    { Http.defaultRequest &
        method: Post,
        headers: [
            Http.header "Authorization" "Bearer $(apiKey)",
        ],
        url: "https://openrouter.ai/api/v1/chat/completions",
        mimeType: "application/json",
        body: buildRequestBody model messages,
        timeout: TimeoutMilliseconds (60 * 1000),
    }


## Build the request body for the OpenRouter API (or any OpenAI style API)
buildRequestBody : Str, List Message -> List U8
buildRequestBody = \model, messages ->
    messagesStr =
        messages
        |> List.map (\{ role, content } -> "{ \"role\": \"$(role)\", \"content\": \"$(content)\"}")
        |> Str.joinWith ", "
    "{ \"model\": \"$(model)\", \"messages\": [ $(messagesStr) ] }" |> Str.toUtf8


## Prompt the user to choose a model and return the selected model
getModelChoice : Task Str _
getModelChoice =
    Stdout.line! modelMenuString
    Stdout.write! "Choose a model (or press enter): "
    choiceStr <- Stdin.line |> Task.map
    Dict.get modelChoices choiceStr
    |> Result.withDefault defaultModel


## The default model selection
defaultModel = "mistralai/mistral-small"


## Define the model choices
modelChoices =
    Dict.empty {}
    |> Dict.insert "1" defaultModel
    |> Dict.insert "2" "mistralai/mistral-large"
    |> Dict.insert "3" "openai/gpt-3.5-turbo"
    |> Dict.insert "4" "openai/gpt-4-turbo"
    |> Dict.insert "5" "microsoft/wizardlm-2-8x22b"


## Generate a string to print for the model selection menu
modelMenuString =
    modelChoices 
    |> Dict.walk "" \string, key, value -> 
        string 
        |> Str.concat key 
        |> Str.concat ") " 
        |> Str.concat value 
        |> Str.concat (if key == "1" then " (default)\n" else "\n")


## Convert a string to lowercase
strToLower : Str -> Str
strToLower = \str ->
    str 
    |> Str.toUtf8 
    |> List.walk [] \acc, elem -> 
        acc |> List.append (if elem >= 65 && elem <= 90 then elem + 32 else elem)
    |> Str.fromUtf8 |> Result.withDefault str