import { fromEvent, merge } from "rxjs";
import { map, mergeScan, first } from "rxjs/operators";
import { ajax } from "rxjs/ajax";
import { type Observable } from "rxjs";
import { State } from "./types";

import hljs from "highlight.js/lib/core";

import highlight from "highlight.js";
import bash from "highlight.js/lib/languages/bash";
import c from "highlight.js/lib/languages/c";
import cpp from "highlight.js/lib/languages/cpp";
import haskell from "highlight.js/lib/languages/haskell";
import css from "highlight.js/lib/languages/css";
import html from "highlight.js/lib/languages/xml";
import java from "highlight.js/lib/languages/java";
import javascript from "highlight.js/lib/languages/javascript";
import json from "highlight.js/lib/languages/json";
import lua from "highlight.js/lib/languages/lua";
import markdown from "highlight.js/lib/languages/markdown";
import php from "highlight.js/lib/languages/php";
import python from "highlight.js/lib/languages/python";
import ruby from "highlight.js/lib/languages/ruby";
import shell from "highlight.js/lib/languages/shell";
import sql from "highlight.js/lib/languages/sql";
import swift from "highlight.js/lib/languages/swift";
import typescript from "highlight.js/lib/languages/typescript";
import yaml from "highlight.js/lib/languages/yaml";

// Load the languages from the unit for syntax highlighting!
highlight.registerLanguage("bash", bash);
highlight.registerLanguage("haskell", haskell);
highlight.registerLanguage("c", c);
highlight.registerLanguage("cpp", cpp);
highlight.registerLanguage("css", css);
highlight.registerLanguage("html", html);
highlight.registerLanguage("java", java);
highlight.registerLanguage("javascript", javascript);
highlight.registerLanguage("json", json);
highlight.registerLanguage("lua", lua);
highlight.registerLanguage("markdown", markdown);
highlight.registerLanguage("php", php);
highlight.registerLanguage("python", python);
highlight.registerLanguage("ruby", ruby);
highlight.registerLanguage("shell", shell);
highlight.registerLanguage("sql", sql);
highlight.registerLanguage("swift", swift);
highlight.registerLanguage("typescript", typescript);
highlight.registerLanguage("yaml", yaml);

const markdownInput = document.getElementById(
    "markdown-input",
) as HTMLTextAreaElement;

const titleInput = document.getElementById(
    "title-input",
) as HTMLTextAreaElement;

const saveButton = document.getElementById(
    "save-button",
) as HTMLTextAreaElement;

const checkbox = document.getElementById("include-style") as HTMLInputElement; 

type Action = (_: State) => State;

const resetState: Action = (s: State) => {
    return { ...s, save: false };
};

const compose =
    <T, U>(g: (_: T) => U) =>
    <V>(f: (_: U) => V) =>
    (t: T): V =>
        f(g(t));

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    markdownInput,
    "input",
).pipe(
    map((event: KeyboardEvent) => (event.target as HTMLInputElement).value),
    map((value: string) => (s: State) => ({ ...s, markdown: value, save: false })),
);

const title$: Observable<Action> = fromEvent<KeyboardEvent>(
    titleInput,
    "input",
).pipe(
    map((event: KeyboardEvent) => (event.target as HTMLInputElement).value),
    map((value: string) => (s: State) => (
        { ...s, 
            title: value.trim().length === 0 ? "Converted HTML" : value,
            save: false
        })),
);

const saveClick$: Observable<Action> = fromEvent<MouseEvent>(saveButton, "click").pipe(
    map(() => (s: State) => (
        { ...s, 
            save: true
        }))
);


const checkboxStream$: Observable<Action> = fromEvent(checkbox, "change").pipe(
    map((event: Event) => (event.target as HTMLInputElement).checked),
    map((value: boolean) => (s: State) => ({ ...s, style: value })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
            "Save-State": s.save,
            "Title-Page": s.title
        },
        body: s.markdown

    }).pipe(
        map((response: any) => response.response), // Extracting the response data
        map((data: any) => {
            return {
                ...s,
                HTML: data.html,
            } as State;
        }),
        first(),
    );
}


function saveHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    return ajax<{ html: string }>({
        url: "/api/convertMD",
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
            "Save-State": s.save,
            "Title-Page": s.title,
            "Style-Css": s.style
        },
        body: s.markdown

    }).pipe(
        map(() =>  ({
            ...s,
            saveSuccess: true
        } as State )),
        first(),
    );
}


const initialState: State = {
    markdown: "",
    HTML: "",
    renderHTML: true,
    style: false,
    title: "Converted HTML",
    save: false,
};

function main() {
    // Subscribe to the input Observable to listen for changes
    const subscription = merge(input$, title$, saveClick$, checkboxStream$)
        .pipe(
            map((reducer: Action) => {
                // Reset Some variables in the state in every tick
                const newState = reducer(initialState);
                return newState.save ? reducer : compose(reducer)(resetState);
            }),
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return newState.save ? saveHTML(newState) : getHTML(newState);
            }, initialState),
        )
        .subscribe((value: State) => {
            const htmlOutput = document.getElementById("html-output") as HTMLElement;
            const raw = document.getElementById("markdown-raw") as HTMLElement;
            const saveStateLabel = document.getElementById("label-status-save") as HTMLElement;
            if (value.save){
                saveStateLabel.textContent = "Sucessfully Save!";
            }
            else{
                saveStateLabel.textContent = "";
            }
            if (htmlOutput) {
                htmlOutput.innerHTML = "";
                htmlOutput.textContent = "";

                raw.innerHTML = "";
                raw.textContent = "";


                // Show them rendered in rendered-output
                const highlight = '<link rel="stylesheet" href="https://unpkg.com/@highlightjs/cdn-assets@11.3.1/styles/default.min.css" />';
                htmlOutput.innerHTML = highlight + value.HTML;
                // Magic code to add code highlighting
                const blocks = htmlOutput.querySelectorAll("pre code");
                blocks.forEach((block) =>
                    hljs.highlightElement(block as HTMLElement),
                );
                
                raw.textContent = value.HTML;
            }
        });
}
if (typeof window !== "undefined") {
    window.onload = function () {
        main();
    };
}
