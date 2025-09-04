include
  [%css
    stylesheet
      {|
        /* --------------------------------------------------------------- ROOT SECTION */
        * {
          margin: 0;
          padding: 0;
          box-sizing: border-box;
        }

        body {
          background-color: #94A3B8;
        }

        /* --------------------------------------------------------------- ROOT SECTION */
        .root {
          display: flex;
          flex-direction: row;
          min-width: 100%;
          font-family: sans-serif;
          color: black;
        }

        /* ---------------------------------------------------------------- NAV SECTION */
        .nav {
          display: flex;
          flex-direction: column;
          width: 20rem;
          justify-content: center;
          align-items: center;
          box-shadow: 2px 0px 10px black;
        }

        /* --------------------------------------------------------------- MAIN SECTION */
        .main {
          display: flex;
          flex-direction: column;
          width: 100%;
        }

        /* ------------------------------------------------------------- HEADER SECTION */
        .header {
          z-index: -1;
          padding: 1rem;
          color: white;
          text-shadow: 0.2rem 0.2rem 0.5rem black;
          min-height: 1rem;
          display: flex;
          flex-direction: row-reverse;
          justify-content: space-between;
        }

        .header-h1 {
          font-size: 2rem;
          display: flex;
          justify-content: center;
          align-items: center;
        }

        /* ------------------------------------------------------------------ MAIN BODY */
        .main-body {
          display: flex;
          flex-direction: column;
          padding: 2rem;
        }

        /* ---------------------------------------------------------- COUNTER COMPONENT */
        .counter-container {
          display: flex;
          justify-content: space-evenly;
          user-select: none;
        }

        .counter {
        }

        .counter-button {
          display: flex;
          justify-content: center;
          align-items: center;
          width: 2ch;
          border: 1px black solid;
          background-color: #334;
          cursor: default;
        }

        .counter-button:hover {
          background-color: #445;
        }

        .counter-button:active {
          background-color: #223;
          color: white;
        }

        /* -------------------------------------------------------------- STATS SECTION */
        .stats-vertical-analysis {
          padding: 2rem;
        }

        .stats-vertical-analysis table {
          border-collapse: collapse;
          font-family: monospace;
        }

        .stats-vertical-analysis table th {
          font-weight: bold;
          color: black;
          padding: 0.25rem;
          border: 1px black solid;
        }

        .stats-vertical-analysis table td {
          color: black;
          padding: 1rem;
          border: 1px black solid;
          text-align: center;
          align-items: center;
        }

        .stats-vertical-analysis table thead th {
          border-top: 0;
        }

        .stats-vertical-analysis table tbody th {
          border-top: 1;
        }

        .stats-vertical-analysis table tr:last-child th,
        .stats-vertical-analysis table tr:last-child td {
          border-bottom: 0;
        }

        .stats-vertical-analysis table tr th:first-child,
        .stats-vertical-analysis table tr td:first-child {
          border-left: 0;
        }

        .stats-vertical-analysis table tr th:last-child,
        .stats-vertical-analysis table tr td:last-child {
          border-right: 0;
        }

        .stats-vertical-analysis-inner-table table {
          border-radius: 0;
          border-collapse: collapse;
          border: 0;
          margin: 0;
        }

        .stats-vertical-analysis-inner-table table td {
          border: 1px black solid;
          padding: 0.5rem;
          color: black;
        }

        .stats-vertical-analysis-inner-table table tr:first-child td {
          border-top: 0;
        }

        .stats-vertical-analysis-inner-table table tr:last-child td {
          border-bottom: 0;
        }

        .stats-vertical-analysis-inner-table table tr td:first-child {
          border-left: 0;
        }

        .stats-vertical-analysis-inner-table table tr td:last-child {
          border-right: 0;
        }

        .keyboard-section {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          font-family: Helvetica;
        }

        .keyboard {
          display: flex;
          flex-direction: column;
          background-color: %{Tailwind_v3_colors.slate700#Css_gen.Color};
          padding: 0.5em;
          border-radius: 1em;
        }

        .keyboard-row {
          display: flex;
          flex-direction: row;
          font-size: 1rem;
          justify-content: space-between;
        }

        .keyboard-key {
          display: flex;
          justify-content: center;
          align-items: center;
          width: var(--keyboard-key-width);
          height: var(--keyboard-key-height);
          background-color: var(--keyboard-key-background-color);
          color: var(--keyboard-key-color);
          margin: 0.1em;
          transition-property: color, background-color;
          transition-timing-function:  cubic-bezier(0.4, 0, 0.2, 1);
          transition-duration: 300ms;
          transition-timing-function:  cubic-bezier(0, 0, 0.2, 1);
          border-radius: 0.5rem;
          box-shadow: 0 25px 50px -12px rgb(0 0 0 / 0.25);
          align-items: center;
          text-align: center;
        }

        .keyboard-key-overlay {
          all: inherit;
          margin: unset;
        }

        .keyboard-key:hover {
          background-color: var(--keyboard-key-hover-background-color);
          color: var(--keyboard-key-hover-color);
        }

        .keyboard-key-label {
          display: flex;
          flex-direction: column;
        }
      |}
      (*~rewrite:
        [ "--keyboard-key-background-color", "--keyboard-key-background-color"
        ; "--keyboard-key-color", "--keyboard-key-color"
        ; "--keyboard-key-hover-background-color", "--keyboard-key-hover-background-color"
        ; "--keyboard-key-hover-color", "--keyboard-key-hover-color"
        ; "--keyboard-background-color", "--keyboard-background-color"
        ]*)
        ]
