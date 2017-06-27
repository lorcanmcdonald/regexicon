import React from "react";
import "whatwg-fetch";
import PropTypes from "prop-types";
import map from "ramda/src/map";

const Examples = () =>
  <div className="examples" key="examples-container">
    <span>
      {"e.g.: "}
    </span>
    <ul>
      <li>
        <a href="/?q=[0-9a-f]{32}">
          {"[0-9a-f]{32}"}
        </a>
      </li>
      <li>
        <a href="/?q=[😀-😊]%2b">
          {"[😀-😊]+"}
        </a>
      </li>
    </ul>
  </div>;

Examples.displayName = "Examples";

const SearchInput = props =>
  <form action="/" key="Search" method="GET">
    <input
      autoFocus
      id="q"
      name="q"
      onKeyUp={props.onKeyUp}
      placeholder="[0-9a-f]{32}"
      value={props.query}
    />
    <button>
      {"🔍"}
    </button>
  </form>;

SearchInput.propTypes = {
  onKeyUp: PropTypes.Func,
  query: PropTypes.String
};

SearchInput.displayName = "SearchInput";

const getRegexCandidates = query =>
  fetch("/regex/?n=5", {
    body: query,
    method: "post"
  }).then(res => res.json());

class Main extends React.Component {
  constructor() {
    super();
    this.handleOnKeyUp = this.handleOnKeyUp.bind(this);
    this.state = { error: [], query: "", results: [] };
  }
  componentWillMount() {
    if (this.props.query) {
      this.setState({ query: this.props.query });
      getRegexCandidates(this.props.query).then(results => {
        if (results.type) {
          this.setState({ error: [results], results: [] });
        } else {
          this.setState({ error: [], results });
        }
      });
    }
  }
  handleOnKeyUp(e) {
    const query = e.target.value;

    this.setState({ query });
    getRegexCandidates(query).then(results => {
      if (results.type) {
        this.setState({ error: [results], results: [] });
      } else {
        this.setState({ error: [], results });
      }
    });
  }
  render() {
    let results = <Examples />;

    if (this.state.error.length) {
      const lines = this.state.error[0].message.split(/\n/);

      results = (
        <div className="error">
          {map(str => <p key={`result-${str}`}>{str}</p>)(lines)}
        </div>
      );
    } else if (this.state.results.length) {
      results = (
        <ul className="results">
          {map(str => <li key={`result-${str}`}>{str}</li>)(this.state.results)}
        </ul>
      );
    }
    return (
      <div>
        <label htmlFor="q">
          {"Generate random strings which match a regular expression"}
        </label>
        <SearchInput onKeyUp={this.handleOnKeyUp} query={this.state.query} />
        {results}
      </div>
    );
  }
}

Main.displayName = "Main";
Main.propTypes = {
  query: PropTypes.String
};

export default Main;
