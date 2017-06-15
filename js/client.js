import React from "react";
import ReactDOM from "react-dom";
import Main from "./Components/Main.jsx";
import queryString from "query-string";

const body = document.getElementsByTagName("body");

const { q } = queryString.parse(window.location.search);

ReactDOM.render(<Main query={q}/>, body[0]);
