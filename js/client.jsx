import React from "react";
import ReactDOM from "react-dom";
import queryString from "query-string";

import Main from "./Components/Main";

const body = document.getElementsByTagName("body");

const { q } = queryString.parse(window.location.search);

ReactDOM.render(<Main query={q} />, body[0]);
