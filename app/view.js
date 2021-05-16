import { h } from '@cycle/dom';
import _ from "lodash";
import moment from "moment";

export default function(model) {

  const enqueueButton = type => h( "button.enqueue-button mdl-button mdl-js-button mdl-button--raised mdl-button--colored", { attrs: {"data-name": type} }, [
      "Enqueue"
  ]);

  const showArgs = xs => h( "p", {}, [
      "Args: " + xs.join(',')
  ]);

  const jobTypes = model.jobTypes.map(x =>
    h( "tr", {}, [
        h( "td", {}, [ x.type ] ),
        h( "td", {}, [ x.description ] ),
        h( "td", {}, [ x.args.length == 0 ? enqueueButton(x.type) : showArgs(x.args) ] )
    ])
  );

  const queue = model.queue.map(x => (
    h( "tr", {}, [
        h( "td", {}, [ x.queueJobType ] ),
        h( "td", {}, [ moment(x.queueTime).format() ] )
    ])
  ));

  const running = model.running.map(x => (
    h( "tr", {}, [
        h( "td", {}, [ x.jobType ] ),
        h( "td", {}, [ x.threadId ] ),
        h( "td", {}, [ x.jobId ] ),
        h( "td", {}, [ moment(x.startTime).format() ] )
    ])
  ));

  const infos = _.flatten(
    model.information.map(cls =>
      cls.values.map(v => (
        h( "tr", {}, [
            h( "td", {}, [ cls.className ] ),
            h( "td", {}, [ v ] )
        ])
      ))
    )
  );

  // let flex = {
  //   display: 'flex',
  //   'flex-wrap': 'wrap'
  // };
  // let flexItem = {
  // };
  // let flexItemBreak = {
  //   'flex-basis': '100%'
  // };
  return (
    h( "div", {}, [
        h( "div.mdl-layout__content mdl-grid" , {}, [
            h( "div.mdl-cell mdl-cell--8-col", { style: { width: '650px' } }, [
                h( "h3", {}, [ "Job Types" ] ),
                h( "table.mdl-data-table mdl-js-data-table mdl-shadow--2dp", {}, [
                    h( "tr", {}, [
                        h( "th", {}, ["Name"] ),
                        h( "th", {}, ["Description"] ),
                        h( "th", {}, ["Action"] ),
                    ]),
                    ...jobTypes
                ])
            ]),
            h( "div.mdl-cell mdl-cell--4-col", {}, [
                h( "h3", {}, [ "Settings" ] ),
                h( "table.mdl-data-table mdl-js-data-table mdl-shadow--2dp", {}, [
                    h( "tr", {}, [
                        h( "th", {}, ["Class"] ),
                        h( "th", {}, ["Information"] ),
                    ]),
                    ...infos
                ])
            ])
        ]),
        h( "div.mdl-layout__content mdl-grid", {}, [
            h( "div.mdl-cell mdl-cell--4-col", { style: { width: '360px' } }, [
                h( "h3", {}, [ "Queue" ] ),
                h( "table.mdl-data-table mdl-js-data-table mdl-shadow--2dp", {}, [
                    h( "tr", {}, [
                        h( "th", {}, ["Type"] ),
                        h( "th", {}, ["Enqueue at"] ),
                    ]),
                    ...queue
                ])
            ]),
            h( "div.mdl-cell mdl-cell--8-col", {}, [
                h( "h3", {}, [ "Running Jobs" ] ),
                h( "table.mdl-data-table mdl-js-data-table mdl-shadow--2dp", {}, [
                    h( "tr", {}, [
                        h( "th", {}, ["Type"] ),
                        h( "th", {}, ["Thread ID"] ),
                        h( "th", {}, ["Job ID"] ),
                        h( "th", {}, ["Start at"] ),
                    ]),
                    ...running
                ])
            ])
        ])
    ])
  );
};
