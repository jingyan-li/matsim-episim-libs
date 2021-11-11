package org.matsim.run.modules;

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Singleton;

import org.matsim.api.core.v01.Scenario;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.groups.VspExperimentalConfigGroup;
import org.matsim.core.controler.ControlerUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.episim.EpisimConfigGroup;
import org.matsim.episim.EpisimUtils;
import org.matsim.episim.TracingConfigGroup;
import org.matsim.episim.EpisimUtils.Extrapolation;
import org.matsim.episim.TracingConfigGroup.CapacityType;
import org.matsim.episim.model.*;
import org.matsim.episim.policy.FixedPolicy;
import org.matsim.episim.policy.Restriction;
import org.matsim.episim.policy.FixedPolicy.ConfigBuilder;
import org.matsim.vehicles.VehicleType;
import com.google.inject.Provides;

public class ZurichScenarioPop100_2021 extends AbstractZurichScenario{

	public static class Builder{
			private int importOffset = 0;
			private int sample = 25;
			private DiseaseImport diseaseImport = DiseaseImport.yes;
			private Restrictions restrictions = Restrictions.yes;
			private Masks masks = Masks.yes;
			private Tracing tracing = Tracing.yes;
			private Snapshot snapshot = Snapshot.no;
			private Class<? extends InfectionModel> infectionModel = AgeDependentInfectionModelWithSeasonality.class;
			private Class<? extends VaccinationModel> vaccinationModel = VaccinationModel.class;
			private boolean withDiseaseImport;

			public Builder setSample( int sample ){
				this.sample = sample;
				return this;
			}
			public Builder setDiseaseImport( DiseaseImport diseaseImport ){
				this.diseaseImport = diseaseImport;
				return this;
			}
			public Builder setRestrictions( Restrictions restrictions ){
				this.restrictions = restrictions;
				return this;
			}
			public Builder setMasks( Masks masks ){
				this.masks = masks;
				return this;
			}
			public Builder setTracing( Tracing tracing ){
				this.tracing = tracing;
				return this;
			}
			public Builder setSnapshot( Snapshot snapshot ){
				this.snapshot = snapshot;
				return this;
			}
			public Builder setInfectionModel( Class<? extends InfectionModel> infectionModel ){
				this.infectionModel = infectionModel;
				return this;
			}

			public ZurichScenarioPop100_2021 createZurichScenarioPop100_2021Scenario(){
				return new ZurichScenarioPop100_2021( sample, diseaseImport, restrictions, masks, tracing, snapshot, infectionModel, importOffset, vaccinationModel,withDiseaseImport );
			}
			public Builder setImportOffset( int importOffset ){
				this.importOffset = importOffset;
				return this;
			}
		}

		public final static class DynamicProgressionModelFactors {

			private static int iteration = 0;

			//https://en.wikipedia.org/wiki/Generalised_logistic_function
			public static double getICUFactor() {

				double A  = 1;
				double K = 0.4;
				double C = 1;
				double Q = 1;
				double B = 0.15;
				double V = 0.9;
				double M = 40;
				//generalized logistic function
				double genlogHospitalFactor = A + ((K-A)/(Math.pow((C+Q*(Math.pow(Math.E,-B*(iteration-M)))),1/V)));
				iteration++ ;
				return genlogHospitalFactor;
			}


		}
//------------------------------------------------------------------------------------------------------------------------------

		/**
		 * Path pointing to the input folder. Can be configured at runtime with EPISIM_INPUT variable.
		 */
		public static Path INPUT = EpisimUtils.resolveInputPath("E:/ETH_Workplace/ABMT/project/data");
		private final int importOffset;

		public static enum DiseaseImport {yes, no}
		public static enum Restrictions {yes, no}
		public static enum Masks {yes, no}
		public static enum Tracing {yes, no}
		public static enum Snapshot {no}

		private final int sample;
		private final DiseaseImport diseaseImport;
		private final Restrictions restrictions;
		private final Masks masks;
		private final Tracing tracing;
		private final Snapshot snapshot;
		private final Class<? extends InfectionModel> infectionModel;
		private final Class<? extends VaccinationModel> vaccinationModel;
		private final boolean withDiseaseImport;


		/**
		 * Empty constructor is needed for running scenario from command line.
		 */

		public ZurichScenarioPop100_2021() {
			this(100, DiseaseImport.no, Restrictions.yes, Masks.yes, Tracing.no, Snapshot.no, AgeDependentInfectionModelWithSeasonality.class, 0, VaccinationByAge.class,true);
		}

		public ZurichScenarioPop100_2021( int sample, DiseaseImport diseaseImport, Restrictions restrictions, Masks masks, Tracing tracing, Snapshot snapshot,
												Class<? extends InfectionModel> infectionModel, int importOffset, Class<? extends VaccinationModel> vaccinationModel,boolean withDiseaseImport ) {
			this.sample = sample;
			this.diseaseImport = diseaseImport;
			this.restrictions = restrictions;
			this.masks = masks;
			this.tracing = tracing;
			this.snapshot = snapshot;
			this.infectionModel = infectionModel;
			this.importOffset = importOffset;
			this.vaccinationModel = vaccinationModel;
			this.withDiseaseImport = withDiseaseImport;
		}

		private static Map<LocalDate, Integer> interpolateImport(Map<LocalDate, Integer> importMap, double importFactor, LocalDate start, LocalDate end, double a, double b) {
			int days = end.getDayOfYear() - start.getDayOfYear();
			for (int i = 1; i <= days; i++) {
				double fraction = (double) i / days;
				importMap.put(start.plusDays(i), (int) Math.round(importFactor * (a + fraction * (b - a))));
			}
			return importMap;
		}

		@Override
		protected void configure() {
			bind(ContactModel.class).to(SymmetricContactModel.class).in(Singleton.class);
			bind(ProgressionModel.class).to(ConfigurableProgressionModel.class).in(Singleton.class);
			bind(InfectionModel.class).to(infectionModel).in(Singleton.class);
			bind(VaccinationModel.class).to(vaccinationModel).in(Singleton.class);

		}

		/**
		 * The base policy based on actual restrictions in the past and mobility data
		 */
		private static FixedPolicy.ConfigBuilder basePolicy(EpisimConfigGroup episimConfig, double alpha,
															Map<String, Double> ciCorrections, Extrapolation extrapolation,
															long introductionPeriod, Double maskCompliance) throws IOException {

			ConfigBuilder restrictions = FixedPolicy.config();

			for (Map.Entry<String, Double> e : ciCorrections.entrySet()) {
				String date = e.getKey();
				Double ciCorrection = e.getValue();
				restrictions.restrict(date, Restriction.ofCiCorrection(ciCorrection), DEFAULT_ACTIVITIES);

			}

			restrictions.restrict(LocalDate.parse("2020-03-16") , 0.1, "education_secondary", "education_higher","education_primary", "education_kiga")
					.restrict(LocalDate.parse("2020-03-18") , 0.72, "work")
					.restrict(LocalDate.parse("2020-03-18") , 0.59, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-03-25") , 0.53, "work")
					.restrict(LocalDate.parse("2020-03-25") , 0.28, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-04-01") , 0.52, "work")
					.restrict(LocalDate.parse("2020-04-01") , 0.24, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-04-08") , 0.53, "work")
					.restrict(LocalDate.parse("2020-04-08") , 0.22, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-04-15") , 0.43, "work")
					.restrict(LocalDate.parse("2020-04-15") , 0.22, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-04-22") , 0.58, "work")
					.restrict(LocalDate.parse("2020-04-22") , 0.27, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-04-29") , 0.61, "work")
					.restrict(LocalDate.parse("2020-04-29") , 0.32, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-05-06") , 0.62, "work")
					.restrict(LocalDate.parse("2020-05-06") , 0.37, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-05-11") , 0.8, "education_secondary","education_primary", "education_kiga")
					.restrict(LocalDate.parse("2020-05-11") , 0.7, "education_higher")
					.restrict(LocalDate.parse("2020-05-13") , 0.72, "work")
					.restrict(LocalDate.parse("2020-05-13") , 0.52, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-05-20") , 0.77, "work")
					.restrict(LocalDate.parse("2020-05-20") , 0.70, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-05-27") , 0.66, "work")
					.restrict(LocalDate.parse("2020-05-27") , 0.72, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-06-03") , 0.75, "work")
					.restrict(LocalDate.parse("2020-06-03") , 0.76, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-06-10") , 0.83, "work")
					.restrict(LocalDate.parse("2020-06-10") , 0.78, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-06-17") , 0.81, "work")
					.restrict(LocalDate.parse("2020-06-17") , 0.81, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-06-24") , 0.84, "work")
					.restrict(LocalDate.parse("2020-06-24") , 0.86, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-07-01") , 0.85, "work")
					.restrict(LocalDate.parse("2020-07-01") , 0.90, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-07-08") , 0.83, "work")
					.restrict(LocalDate.parse("2020-07-08") , 0.92, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-07-15") , 0.79, "work")
					.restrict(LocalDate.parse("2020-07-15") , 0.90, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-07-22") , 0.76, "work")
					.restrict(LocalDate.parse("2020-07-22") , 0.89, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-07-29") , 0.72, "work")
					.restrict(LocalDate.parse("2020-07-29") , 0.90, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-08-05") , 0.66, "work")
					.restrict(LocalDate.parse("2020-08-05") , 0.83, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-08-12") , 0.75, "work")
					.restrict(LocalDate.parse("2020-08-12") , 0.88, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-08-19") , 0.79, "work")
					.restrict(LocalDate.parse("2020-08-19") , 0.89, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-08-26") , 0.82, "work")
					.restrict(LocalDate.parse("2020-08-26") , 0.90, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-09-02") , 0.81, "work")
					.restrict(LocalDate.parse("2020-09-02") , 0.87, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-09-11") , 1.0, "education_secondary","education_primary", "education_kiga","education_higher")
					.restrict(LocalDate.parse("2020-09-09") , 0.82, "work")
					.restrict(LocalDate.parse("2020-09-09") , 0.90, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-09-16") , 0.84, "work")
					.restrict(LocalDate.parse("2020-09-16") , 0.91, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-09-23") , 0.83, "work")
					.restrict(LocalDate.parse("2020-09-23") , 0.88, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-09-30") , 0.83, "work")
					.restrict(LocalDate.parse("2020-09-30") , 0.87, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-10-07") , 0.81, "work")
					.restrict(LocalDate.parse("2020-10-07") , 0.88, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-10-14") , 0.81, "work")
					.restrict(LocalDate.parse("2020-10-14") , 0.85, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-10-21") , 0.82, "work")
					.restrict(LocalDate.parse("2020-10-21") , 0.81, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-10-28") , 0.82, "work")
					.restrict(LocalDate.parse("2020-10-28") , 0.79, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-11-04") , 0.81, "work")
					.restrict(LocalDate.parse("2020-11-04") , 0.74, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-11-11") , 0.79, "work")
					.restrict(LocalDate.parse("2020-11-11") , 0.69, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-11-18") , 0.81, "work")
					.restrict(LocalDate.parse("2020-11-18") , 0.70, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-11-25") , 0.81, "work")
					.restrict(LocalDate.parse("2020-11-25") , 0.70, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-12-02") , 0.83, "work")
					.restrict(LocalDate.parse("2020-12-02") , 0.75, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-12-09") , 0.81, "work")
					.restrict(LocalDate.parse("2020-12-09") , 0.71, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-12-16") , 0.83, "work")
					.restrict(LocalDate.parse("2020-12-16") , 0.75, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-12-23") , 0.75, "work")
					.restrict(LocalDate.parse("2020-12-23") , 0.81, "shop","leisure","other")
					.restrict(LocalDate.parse("2020-12-30") , 0.75, "work")
					.restrict(LocalDate.parse("2020-12-30") , 0.81, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-01-06") , 0.55, "work")
					.restrict(LocalDate.parse("2021-01-06") , 0.46, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-01-13") , 0.76, "work")
					.restrict(LocalDate.parse("2021-01-13") , 0.55, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-01-20") , 0.74, "work")
					.restrict(LocalDate.parse("2021-01-20") , 0.49, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-01-27") , 0.74, "work")
					.restrict(LocalDate.parse("2021-01-27") , 0.46, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-02-03") , 0.74, "work")
					.restrict(LocalDate.parse("2021-02-03") , 0.46, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-02-10") , 0.74, "work")
					.restrict(LocalDate.parse("2021-02-10") , 0.46, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-02-17") , 0.73, "work")
					.restrict(LocalDate.parse("2021-02-17") , 0.50, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-02-24") , 0.75, "work")
					.restrict(LocalDate.parse("2021-02-24") , 0.55, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-03-03") , 0.78, "work")
					.restrict(LocalDate.parse("2021-03-03") , 0.61, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-03-10") , 0.80, "work")
					.restrict(LocalDate.parse("2021-03-10") , 0.64, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-03-17") , 0.80, "work")
					.restrict(LocalDate.parse("2021-03-17") , 0.60, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-03-24") , 0.81, "work")
					.restrict(LocalDate.parse("2021-03-24") , 0.63, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-03-31") , 0.83, "work")
					.restrict(LocalDate.parse("2021-03-31") , 0.71, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-04-07") , 0.8, "work")
					.restrict(LocalDate.parse("2021-04-07") , 0.69, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-04-14") , 0.78, "work")
					.restrict(LocalDate.parse("2021-04-14") , 0.67, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-04-21") , 0.80, "work")
					.restrict(LocalDate.parse("2021-04-21") , 0.70, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-04-28") , 0.84, "work")
					.restrict(LocalDate.parse("2021-04-28") , 0.80, "shop","leisure","other")
					.restrict(LocalDate.parse("2021-05-10") , 0.9, "work")
					.restrict(LocalDate.parse("2021-05-10") , 0.9, "shop","leisure","other");


			//MASKS

			LocalDate masksFirstDay = LocalDate.of(2020, 3, 20);
			int introductionPeriodMask = 160;
			int midpointPt = 80;
			int midpointShop = 80;
			int midpointWork = 80;
			int midpointLeisure = 80;
			double maxvalueMaskPt = 0.95;
			double maxvalueMaskShop = 0.95;
			double maxvalueMaskWork = 0.55;
			double maxvalueMaskLeisure = 0.15;
			double logGrowthRatePt = 0.05;
			double logGrowthRateShop = 0.07;
			double logGrowthRateWork = 0.07;
			double logGrowthRateLeisure = 0.07;
			for (int ii = 0; ii <= introductionPeriodMask; ii++) {
				LocalDate date = masksFirstDay.plusDays(ii);
				double logResPt = maxvalueMaskPt/(1+Math.pow(Math.E,-logGrowthRatePt*(ii- midpointPt)));
				double logResShop = maxvalueMaskShop/(1+Math.pow(Math.E,-logGrowthRateShop*(ii- midpointShop)));
				double logResWork = maxvalueMaskWork/(1+Math.pow(Math.E,-logGrowthRateWork*(ii- midpointWork)));
				double logResLeisure = maxvalueMaskLeisure/(1+Math.pow(Math.E,-logGrowthRateLeisure*(ii- midpointLeisure)));
				restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResPt * 0.9, FaceMask.SURGICAL, logResPt * 0.1)), "pt","tr");
				restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResShop * 0.9, FaceMask.SURGICAL, logResShop * 0.1)), "shop","service");
				restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResWork * 0.9, FaceMask.SURGICAL, logResWork * 0.1)), "work");
				restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResLeisure * 0.9, FaceMask.SURGICAL, logResLeisure * 0.1)), "leisure","other");
			}
			restrictions.restrict("2020-06-07", Restriction.ofMask(Map.of(FaceMask.CLOTH, 0.95 * 0.9, FaceMask.SURGICAL, 0.95 * 0.1)), "pt","tr");
			restrictions.restrict("2020-08-26", Restriction.ofMask(Map.of(FaceMask.CLOTH, 0.95 * 0.9, FaceMask.SURGICAL, 0.95 * 0.1)), "pt","tr" ,"shop","service");
			return restrictions;
		}

		@Provides
		@Singleton
		public Config config() {

			if (this.sample != 25) throw new RuntimeException("Sample size not calibrated! Currently only 25% is calibrated. Comment this line out to continue.");

			Config config = getBaseConfig();

			EpisimConfigGroup episimConfig = ConfigUtils.addOrGetModule(config, EpisimConfigGroup.class);
			episimConfig.setInputEventsFile(INPUT.resolve("output_events.xml.gz").toString());
			config.plans().setInputFile(INPUT.resolve("output_plans.xml.gz").toString());
			//episimConfig.setInitialInfections(1800);
			//episimConfig.setInitialInfectionDistrict("Ticino");
			episimConfig.setSampleSize(0.25);
			episimConfig.setCalibrationParameter(1.05E-5);
			episimConfig.setMaxContacts(4);
			String startDate = "2020-02-22";
			episimConfig.setStartDate(startDate);
			episimConfig.setHospitalFactor(1);
			episimConfig.setProgressionConfig(baseProgressionConfig(Transition.config()).build());


			//tracing
			if (this.tracing == Tracing.yes) {
				TracingConfigGroup tracingConfig = ConfigUtils.addOrGetModule(config, TracingConfigGroup.class);
				int offset = (int) (ChronoUnit.DAYS.between(episimConfig.getStartDate(), LocalDate.parse("2020-04-01")) + 1);
				tracingConfig.setPutTraceablePersonsInQuarantineAfterDay(offset);
				tracingConfig.setTracingProbability(0.5);
				tracingConfig.setTracingPeriod_days(2);
				tracingConfig.setMinContactDuration_sec(15 * 60.);
				tracingConfig.setQuarantineHouseholdMembers(true);
				tracingConfig.setEquipmentRate(1.);
				tracingConfig.setTracingDelay_days(5);
				tracingConfig.setTraceSusceptible(true);
				tracingConfig.setCapacityType(CapacityType.PER_PERSON);
				int tracingCapacity = 60;
				tracingConfig.setTracingCapacity_pers_per_day(Map.of(
						LocalDate.of(2020, 4, 1), (int) (0.2 * tracingCapacity),
						LocalDate.of(2020, 6, 15), tracingCapacity
				));
			}

			BasePolicyBuilder basePolicyBuilder = new BasePolicyBuilder(episimConfig);

			double ciCorrCorr = 0.65;
			double xyz = 0.5;
			basePolicyBuilder.setCiCorrections(Map.of(
					"2020-04-01",0.5*ciCorrCorr,
					"2020-07-15", 0.3*ciCorrCorr,
					"2020-09-01", 0.8*ciCorrCorr,
					"2020-09-15", 1.0*ciCorrCorr,
					"2020-11-05", 1.0*ciCorrCorr*xyz,
					"2021-01-08", 1.0*ciCorrCorr,
					"2021-02-23", 1.0*ciCorrCorr,
					"2021-03-20", 0.8*ciCorrCorr,
					"2021-04-01",0.6*ciCorrCorr,
					"2021-07-15", 0.3*ciCorrCorr
			));




			FixedPolicy.ConfigBuilder builder = basePolicyBuilder.build();
			episimConfig.setPolicy(FixedPolicy.class, builder.build());


			if (withDiseaseImport) {
				//SARS_CoV_2
				Map<LocalDate, Integer> importMapSARS_CoV_2 = new HashMap<>();
				double importFactor = 1.;
				importMapSARS_CoV_2.put(episimConfig.getStartDate(), Math.max(1, (int) Math.round(0.9 * importFactor)));
				int importOffset = 0;
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-02-23").plusDays(importOffset),
						LocalDate.parse("2020-03-15").plusDays(importOffset), 80, 60);
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-03-16").plusDays(importOffset),
						LocalDate.parse("2020-04-01").plusDays(importOffset), 60, 5);
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-05-26").plusDays(importOffset),
						LocalDate.parse("2020-07-26").plusDays(importOffset), 15, 30);
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-07-27").plusDays(importOffset),
						LocalDate.parse("2020-08-20").plusDays(importOffset), 35, 35);
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-08-21").plusDays(importOffset),
						LocalDate.parse("2020-09-15").plusDays(importOffset), 35, 30);
				importMapSARS_CoV_2 = interpolateImport(importMapSARS_CoV_2, importFactor, LocalDate.parse("2020-09-16").plusDays(importOffset),
						LocalDate.parse("2020-11-01").plusDays(importOffset), 30, 15);
				episimConfig.setInfections_pers_per_day(VirusStrain.SARS_CoV_2,importMapSARS_CoV_2);


				Map<LocalDate, Integer> importMapB117 = new HashMap<>();
				importMapB117.put(episimConfig.getStartDate(), Math.max(1, (int) Math.round(0.9 * importFactor)));
				importMapB117 = interpolateImport(importMapB117, importFactor, LocalDate.parse("2020-09-15").plusDays(importOffset),
						LocalDate.parse("2020-11-01").plusDays(importOffset), 10, 25);
				importMapB117 = interpolateImport(importMapB117, importFactor, LocalDate.parse("2021-01-26").plusDays(importOffset),
						LocalDate.parse("2021-03-10").plusDays(importOffset), 5, 25);
				importMapB117 = interpolateImport(importMapB117, importFactor, LocalDate.parse("2021-03-11").plusDays(importOffset),
						LocalDate.parse("2021-07-26").plusDays(importOffset), 25, 40);
				episimConfig.setInfections_pers_per_day(VirusStrain.B117,importMapB117);
			}
			return config;
		}


		@Provides
		@Singleton
		public Scenario scenario(Config config) {

			// guice will use no args constructor by default, we check if this config was initialized
			// this is only the case when no explicit binding are required
			if (config.getModules().size() == 0)
				throw new IllegalArgumentException("Please provide a config module or binding.");

			config.vspExperimental().setVspDefaultsCheckingLevel(VspExperimentalConfigGroup.VspDefaultsCheckingLevel.warn);

			// save some time for not needed inputs
			config.facilities().setInputFile(null);

			ControlerUtils.checkConfigConsistencyAndWriteToLog(config, "before loading scenario");

			final Scenario scenario = ScenarioUtils.loadScenario(config);

			double capFactor = 1.3;

			for (VehicleType vehicleType : scenario.getVehicles().getVehicleTypes().values()) {
				switch (vehicleType.getId().toString()) {
					case "BUS":
						vehicleType.getCapacity().setSeats((int) (70 * capFactor));
						vehicleType.getCapacity().setStandingRoom((int) (40 * capFactor));
						// https://de.wikipedia.org/wiki/Stadtbus_(Fahrzeug)#Stehpl%C3%A4tze
						break;
//				case "metro":
//					vehicleType.getCapacity().setSeats((int) (200 * capFactor));
//					vehicleType.getCapacity().setStandingRoom((int) (550 * capFactor));
//					// https://mein.berlin.de/ideas/2019-04585/#:~:text=Ein%20Vollzug%20der%20Baureihe%20H,mehr%20Stehpl%C3%A4tze%20zur%20Verf%C3%BCgung%20stehen.
//					break;
//				case "plane":
//					vehicleType.getCapacity().setSeats((int) (200 * capFactor));
//					vehicleType.getCapacity().setStandingRoom((int) (0 * capFactor));
//					break;
//				case "pt":
//					vehicleType.getCapacity().setSeats((int) (70 * capFactor));
//					vehicleType.getCapacity().setStandingRoom((int) (70 * capFactor));
//					break;
//				case "ship":
//					vehicleType.getCapacity().setSeats((int) (150 * capFactor));
//					vehicleType.getCapacity().setStandingRoom((int) (150 * capFactor));
//					// https://www.berlin.de/tourismus/dampferfahrten/faehren/1824948-1824660-faehre-f10-wannsee-altkladow.html
//					break;
					case "ZUG":
						vehicleType.getCapacity().setSeats((int) (400 * capFactor));
						vehicleType.getCapacity().setStandingRoom((int) (0 * capFactor));
						// https://de.wikipedia.org/wiki/Stadler_KISS#Technische_Daten_der_Varianten , mehr als ICE (https://inside.bahn.de/ice-baureihen/)
						break;
//				case "tram":
//					vehicleType.getCapacity().setSeats((int) (84 * capFactor));
//					vehicleType.getCapacity().setStandingRoom((int) (216 * capFactor));
//					// https://mein.berlin.de/ideas/2019-04585/#:~:text=Ein%20Vollzug%20der%20Baureihe%20H,mehr%20Stehpl%C3%A4tze%20zur%20Verf%C3%BCgung%20stehen.
//					break;
					default:
						throw new IllegalStateException("Unexpected value=|" + vehicleType.getId().toString() + "|");
				}
			}

			return scenario;
		}


		public static class BasePolicyBuilder {
			private final EpisimConfigGroup episimConfig;

			/*
			 *  alpha = 1 -> ci=0.323
			 *  alpha = 1.2 -> ci=0.360
			 *  alpha = 1.4 -> ci=0.437
			 */
			//?
			private Map<String, Double> ciCorrections = Map.of("2020-03-12", 0.26); //0.34
			private double alpha = 1.;
			private Extrapolation extrapolation = Extrapolation.none;
			private long introductionPeriod = 14;
			private double maskCompliance = 0.95;

			public BasePolicyBuilder(EpisimConfigGroup episimConfig) {
				this.episimConfig = episimConfig;
			}

			public void setIntroductionPeriod(long introductionPeriod) {
				this.introductionPeriod = introductionPeriod;
			}

			public void setMaskCompliance(double maskCompliance) {
				this.maskCompliance = maskCompliance;
			}

			public void setCsv(Path csv) {
				//this.csv = csv;
			}

			public double getAlpha() {
				return alpha;
			}

			public void setAlpha(double alpha) {
				this.alpha = alpha;
			}

			public void setCiCorrections(Map<String, Double> ciCorrections) {
				this.ciCorrections = ciCorrections;
			}

			public Map<String, Double> getCiCorrections() {
				return ciCorrections;
			}

			public Extrapolation getExtrapolation() {
				return extrapolation;
			}

			public void setExtrapolation(Extrapolation extrapolation) {
				this.extrapolation = extrapolation;
			}

			public ConfigBuilder build() {
				ConfigBuilder configBuilder = null;
				try {
					configBuilder = basePolicy(episimConfig, alpha, ciCorrections, extrapolation, introductionPeriod,
							maskCompliance);
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
				return configBuilder;
			}
		}
}
