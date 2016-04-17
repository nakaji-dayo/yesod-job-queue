/** @jsx hJSX */
import _ from "lodash";
import Cycle from '@cycle/core';
import {makeDOMDriver, hJSX} from '@cycle/dom';

export default function(x) {
  console.log(x);
  const jobTypes = x.jobTypes.map(x => (
    <tr>
        <td>{x.type}</td>
        <td>
            <button className="enqueue-button mdl-button mdl-js-button mdl-button--raised mdl-button--colored" attributes={{"data-name": x.type}}>
                enqueue
            </button>
        </td>
    </tr>
  ));
  const queue = x.queue.map(x => (
    <tr>
        <td>{x}</td>
    </tr>
  ));
  const running = x.running.map(x => (
    <tr>
        <td>{x}</td>
    </tr>
  ));

  return (
    <div className="mdl-layout__content mdl-grid">
        <div className="mdl-cell mdl-cell--12-col">
            <h3>Running Jobs</h3>
            <table className="mdl-data-table mdl-js-data-table mdl-shadow--2dp">
                <tr><th>Job ID</th></tr>
                {running}
            </table>

            <h3>Queue</h3>
            <table className="mdl-data-table mdl-js-data-table mdl-shadow--2dp">
                <tr><th>name</th></tr>
                {queue}
            </table>
            <h3>Job Types</h3>
            <table className="mdl-data-table mdl-js-data-table mdl-shadow--2dp">
                <tr><th>name</th><th>action</th></tr>
                {jobTypes}
            </table>
        </div>
        {/*Snackbar*/}
    </div>
  );
}
