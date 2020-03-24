package org.matsim.episim;

import com.google.common.base.Joiner;
import org.apache.log4j.Logger;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.population.Person;
import org.matsim.core.config.Config;
import org.matsim.core.utils.io.IOUtils;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Map;

class EpisimReporting{
        enum InfectionsWriterFields{ time, nTotalInfected, nInQuarantine, nRecovered, nSusceptible, nContagious, nInfectedButNotContagious,
                nInfectedCumulative, nSeriouslySick, nCritical }

        enum InfectionEventsWriterFields{ time, infector, infected, infectionType }

        private static final Logger log = Logger.getLogger( EpisimReporting.class );
        private static int specificInfectionsCnt = 300;
        private static Joiner separator = Joiner.on("\t");
        private final BufferedWriter infectionsWriter;
        private final BufferedWriter infectionEventsWriter;
        EpisimReporting( Config config ){
                String base ;
                if ( config.controler().getRunId()!=null ) {
                        base = config.controler().getOutputDirectory() + "/" + config.controler().getRunId() + "." ;
                } else {
                        base = config.controler().getOutputDirectory() + "/" ;
                }
                infectionsWriter = prepareWriter( base + "infections.txt", InfectionsWriterFields.class );
                infectionEventsWriter = prepareWriter( base + "infectionEvents.txt" , InfectionEventsWriterFields.class );
        }

        void reporting( Map<Id<Person>, EpisimPerson> personMap, int iteration ){
        		if (iteration == 0) {
        			return;
        		}
                long nSusceptible = 0;
                long nInfectedButNotContagious = 0;
                long nContagious = 0;
                long nRecovered = 0;
                long nQuarantined = 0;
                long nSeriouslySick = 0;
                long nCritical = 0;
                long nTotalInfected = 0;
                for( EpisimPerson person : personMap.values() ){
                        switch( person.getDiseaseStatus() ) {
                                case susceptible:
                                        nSusceptible++;
                                        break;
                                case infectedButNotContagious:
                                        nInfectedButNotContagious++;
                                        nTotalInfected++;
                                        break;
                                case contagious:
                                        nContagious++;
                                        nTotalInfected++;
                                        break;
                                case seriouslySick:
                                        nSeriouslySick++;
                                        nTotalInfected++;
                                        break;
                                case critical:
                                        nCritical++;
                                        nTotalInfected++;
                                        break;
                                case recovered:
                                        nRecovered++;
                                        break;
                                default:
                                        throw new IllegalStateException( "Unexpected value: " + person.getDiseaseStatus() );
                        }
                        switch( person.getQuarantineStatus() ) {
                                case full:
                                        nQuarantined++;
                                        break;
                                case no:
                                        break;
                                default:
                                        throw new IllegalStateException( "Unexpected value: " + person.getQuarantineStatus() );
                        }
                }

                log.warn("===============================" );
                log.warn("Beginning day " + iteration );
                log.warn("No of susceptible persons=" + nSusceptible + " / " + 100 * nSusceptible / (nSusceptible + nTotalInfected + nRecovered) + "%");
                log.warn( "No of infected persons=" + nTotalInfected + " / " + 100 * nTotalInfected / (nSusceptible + nTotalInfected + nRecovered) + "%");
                log.warn( "No of recovered persons=" + nRecovered + " / " + 100 * nRecovered / (nSusceptible + nTotalInfected + nRecovered) + "%");
                log.warn( "---" );
                log.warn( "No of persons in quarantine=" + nQuarantined );
                log.warn("===============================" );

                String[] array = new String[InfectionsWriterFields.values().length];

                array[InfectionsWriterFields.time.ordinal()] = Double.toString( EpisimUtils.getCorrectedTime( 0.,iteration ) );
                array[InfectionsWriterFields.nSusceptible.ordinal()] = Long.toString( nSusceptible );
                array[InfectionsWriterFields.nInfectedButNotContagious.ordinal()] = Long.toString( nInfectedButNotContagious );
                array[InfectionsWriterFields.nContagious.ordinal()] = Long.toString( nContagious );
                array[InfectionsWriterFields.nRecovered.ordinal()] = Long.toString( nRecovered );

                array[InfectionsWriterFields.nTotalInfected.ordinal()] = Long.toString( (nTotalInfected) ) ;
                array[InfectionsWriterFields.nInfectedCumulative.ordinal()] = Long.toString( (nTotalInfected + nRecovered) );

                array[InfectionsWriterFields.nInQuarantine.ordinal()] = Long.toString( nQuarantined );

                array[InfectionsWriterFields.nSeriouslySick.ordinal()] = Long.toString( nSeriouslySick );
                array[InfectionsWriterFields.nCritical.ordinal()] = Long.toString( nCritical );

                write( array, infectionsWriter );
        }
        private static void write( String[] array, BufferedWriter writer ){
                try{
                        writer.write(separator.join(array));
                        writer.newLine();
                        writer.flush();
                } catch( IOException e ){
                        throw new RuntimeException( e );
                }
        }
        void reportInfection( EpisimPerson personWrapper, EpisimPerson infector, double now, String infectionType ){
                if ( specificInfectionsCnt-- > 0 ){
                        log.warn( "infection of personId=" + personWrapper.getPersonId() + " by person=" + infector.getPersonId() + " at/in " + infectionType );
                }
                {
                        String[] array = new String[InfectionEventsWriterFields.values().length];
                        array[InfectionEventsWriterFields.time.ordinal()] = Double.toString( now );
                        array[InfectionEventsWriterFields.infector.ordinal()] = infector.getPersonId().toString();
                        array[InfectionEventsWriterFields.infected.ordinal()] = personWrapper.getPersonId().toString();
                        array[InfectionEventsWriterFields.infectionType.ordinal()] = infectionType;

                        write( array, infectionEventsWriter );
                }
        }
        private static BufferedWriter prepareWriter( String filename, Class<? extends Enum<?>> enumClass ){
                BufferedWriter writer = IOUtils.getBufferedWriter( filename );
                try{
                        writer.write(separator.join(enumClass.getEnumConstants()));
                        writer.newLine();
                } catch( IOException e ){
                        throw new RuntimeException( e );
                }
                return writer;
        }
}