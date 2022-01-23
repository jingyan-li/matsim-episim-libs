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
import org.matsim.episim.model.progression.AgeDependentDiseaseStatusTransitionModel;
import org.matsim.episim.model.progression.DefaultDiseaseStatusTransitionModel;
import org.matsim.episim.model.progression.DiseaseStatusTransitionModel;
import org.matsim.episim.policy.FixedPolicy;
import org.matsim.episim.policy.Restriction;
import org.matsim.episim.policy.FixedPolicy.ConfigBuilder;
import org.matsim.vehicles.VehicleType;
import com.google.inject.Provides;




public class ZurichScenarioPop100_2021 extends AbstractZurichScenario{

	/**
	 * Path pointing to the input folder. Can be configured at runtime with EPISIM_INPUT variable.
	 */
	public static Path INPUT = EpisimUtils.resolveInputPath("//cluster/work/ivt_vpl/jingyli/episim/data_25pt");
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

	/**
	 * Constructor
	 */
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

	/**
	 * The models IMPL used in Episim must be bound to their interfaces.
	 */
	@Override
	protected void configure() {
		bind(ContactModel.class).to(SymmetricContactModel.class).in(Singleton.class);
//		bind(ProgressionModel.class).to(AgeDependentProgressionModel.class).in(Singleton.class);
//		bind(InfectionModel.class).to(infectionModel).in(Singleton.class);
		bind(VaccinationModel.class).to(vaccinationModel).in(Singleton.class);
//		bind(DiseaseStatusTransitionModel.class).to(AgeDependentDiseaseStatusTransitionModel.class).in(Singleton.class);
		bind(DiseaseStatusTransitionModel.class).to(AgeDependentDiseaseStatusTransitionModel.class).in(Singleton.class);
		bind(InfectionModel.class).to(AgeDependentInfectionModelWithSeasonality.class).in(Singleton.class);
	}


	/**
	 * The base policy based on restrictions and mobility data (1)
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
		restrictions.restrict(LocalDate.parse("2020-02-25"),0.96,"work")
				.restrict(LocalDate.parse("2020-02-25"),0.97,"shop","leisure","other")
				.restrict(LocalDate.parse("2020-03-03"),0.98,"work")
				.restrict(LocalDate.parse("2020-03-03"),0.97,"shop","leisure","other")
				.restrict(LocalDate.parse("2020-03-10"),0.97,"work")
				.restrict(LocalDate.parse("2020-03-10"),0.92,"shop","leisure","other")
		// lock down
//				.restrict(LocalDate.parse("2020-03-17"),0.85,"work")
//				.restrict(LocalDate.parse("2020-03-17"),0.77,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-03-24"),0.52,"work")
//				.restrict(LocalDate.parse("2020-03-24"),0.31,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-03-31"),0.49,"work")
//				.restrict(LocalDate.parse("2020-03-31"),0.27,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-07"),0.50,"work")
//				.restrict(LocalDate.parse("2020-04-07"),0.20,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-14"),0.40,"work")
//				.restrict(LocalDate.parse("2020-04-14"),0.22,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-21"),0.52,"work")
//				.restrict(LocalDate.parse("2020-04-21"),0.28,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-28"),0.55,"work")
//				.restrict(LocalDate.parse("2020-04-28"),0.32,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-05"),0.53,"work")
//				.restrict(LocalDate.parse("2020-05-05"),0.35,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-12"),0.65,"work")
//				.restrict(LocalDate.parse("2020-05-12"),0.49,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-19"),0.70,"work")
//				.restrict(LocalDate.parse("2020-05-19"),0.64,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-26"),0.61,"work")
//				.restrict(LocalDate.parse("2020-05-26"),0.66,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-02"),0.69,"work")
//				.restrict(LocalDate.parse("2020-06-02"),0.70,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-09"),0.76,"work")
//				.restrict(LocalDate.parse("2020-06-09"),0.75,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-16"),0.78,"work")
//				.restrict(LocalDate.parse("2020-06-16"),0.80,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-23"),0.79,"work")
//				.restrict(LocalDate.parse("2020-06-23"),0.81,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-30"),0.80,"work")
//				.restrict(LocalDate.parse("2020-06-30"),0.84,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-07"),0.82,"work")
//				.restrict(LocalDate.parse("2020-07-07"),0.86,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-14"),0.78,"work")
//				.restrict(LocalDate.parse("2020-07-14"),0.85,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-21"),0.73,"work")
//				.restrict(LocalDate.parse("2020-07-21"),0.80,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-28"),0.70,"work")
//				.restrict(LocalDate.parse("2020-07-28"),0.79,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-04"),0.63,"work")
//				.restrict(LocalDate.parse("2020-08-04"),0.72,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-11"),0.70,"work")
//				.restrict(LocalDate.parse("2020-08-11"),0.80,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-18"),0.75,"work")
//				.restrict(LocalDate.parse("2020-08-18"),0.84,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-25"),0.77,"work")
//				.restrict(LocalDate.parse("2020-08-25"),0.85,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-01"),0.76,"work")
//				.restrict(LocalDate.parse("2020-09-01"),0.83,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-08"),0.76,"work")
//				.restrict(LocalDate.parse("2020-09-08"),0.85,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-15"),0.78,"work")
//				.restrict(LocalDate.parse("2020-09-15"),0.87,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-22"),0.81,"work")
//				.restrict(LocalDate.parse("2020-09-22"),0.86,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-29"),0.81,"work")
//				.restrict(LocalDate.parse("2020-09-29"),0.85,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-10-06"),0.78,"work")
//				.restrict(LocalDate.parse("2020-10-06"),0.88,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-10-13"),0.72,"work")
//				.restrict(LocalDate.parse("2020-10-13"),0.79,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-10-20"),0.75,"work")
//				.restrict(LocalDate.parse("2020-10-20"),0.78,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-10-27"),0.78,"work")
//				.restrict(LocalDate.parse("2020-10-27"),0.79,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-11-03"),0.76,"work")
//				.restrict(LocalDate.parse("2020-11-03"),0.73,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-11-10"),0.77,"work")
//				.restrict(LocalDate.parse("2020-11-10"),0.75,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-11-17"),0.78,"work")
//				.restrict(LocalDate.parse("2020-11-17"),0.77,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-11-24"),0.78,"work")
//				.restrict(LocalDate.parse("2020-11-24"),0.76,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-12-01"),0.79,"work")
//				.restrict(LocalDate.parse("2020-12-01"),0.83,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-12-08"),0.80,"work")
//				.restrict(LocalDate.parse("2020-12-08"),0.81,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-12-15"),0.78,"work")
//				.restrict(LocalDate.parse("2020-12-15"),0.73,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-12-22"),0.72,"work")
//				.restrict(LocalDate.parse("2020-12-22"),0.76,"shop","leisure","other")
//				.restrict(LocalDate.parse("2020-12-29"),0.40,"work")
//				.restrict(LocalDate.parse("2020-12-29"),0.47,"shop","leisure","other");
				.restrict(LocalDate.parse("2020-03-16") , 0.1, "education")
				.restrict(LocalDate.parse("2020-05-11") , 0.7, "education")
				.restrict(LocalDate.parse("2020-09-11") , 1.0, "education");

//		restrictions.restrict(LocalDate.parse("2020-03-16") , 0.1, "education") //todo: definition of fraction
//				.restrict(LocalDate.parse("2020-03-18") , 0.72, "work")
//				.restrict(LocalDate.parse("2020-03-18") , 0.59, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-03-25") , 0.53, "work")
//				.restrict(LocalDate.parse("2020-03-25") , 0.28, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-01") , 0.52, "work")
//				.restrict(LocalDate.parse("2020-04-01") , 0.24, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-08") , 0.53, "work")
//				.restrict(LocalDate.parse("2020-04-08") , 0.22, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-15") , 0.43, "work")
//				.restrict(LocalDate.parse("2020-04-15") , 0.22, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-22") , 0.58, "work")
//				.restrict(LocalDate.parse("2020-04-22") , 0.27, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-04-29") , 0.61, "work")
//				.restrict(LocalDate.parse("2020-04-29") , 0.32, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-06") , 0.62, "work")
//				.restrict(LocalDate.parse("2020-05-06") , 0.37, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-11") , 0.8, "education")
//				.restrict(LocalDate.parse("2020-05-11") , 0.7, "education")
//				.restrict(LocalDate.parse("2020-05-13") , 0.72, "work")
//				.restrict(LocalDate.parse("2020-05-13") , 0.52, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-20") , 0.77, "work")
//				.restrict(LocalDate.parse("2020-05-20") , 0.70, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-05-27") , 0.66, "work")
//				.restrict(LocalDate.parse("2020-05-27") , 0.72, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-03") , 0.75, "work")
//				.restrict(LocalDate.parse("2020-06-03") , 0.76, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-10") , 0.83, "work")
//				.restrict(LocalDate.parse("2020-06-10") , 0.78, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-17") , 0.81, "work")
//				.restrict(LocalDate.parse("2020-06-17") , 0.81, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-06-24") , 0.84, "work")
//				.restrict(LocalDate.parse("2020-06-24") , 0.86, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-01") , 0.85, "work")
//				.restrict(LocalDate.parse("2020-07-01") , 0.90, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-08") , 0.83, "work")
//				.restrict(LocalDate.parse("2020-07-08") , 0.92, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-15") , 0.79, "work")
//				.restrict(LocalDate.parse("2020-07-15") , 0.90, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-22") , 0.76, "work")
//				.restrict(LocalDate.parse("2020-07-22") , 0.89, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-07-29") , 0.72, "work")
//				.restrict(LocalDate.parse("2020-07-29") , 0.90, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-05") , 0.66, "work")
//				.restrict(LocalDate.parse("2020-08-05") , 0.83, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-12") , 0.75, "work")
//				.restrict(LocalDate.parse("2020-08-12") , 0.88, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-19") , 0.79, "work")
//				.restrict(LocalDate.parse("2020-08-19") , 0.89, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-08-26") , 0.82, "work")
//				.restrict(LocalDate.parse("2020-08-26") , 0.90, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-02") , 0.81, "work")
//				.restrict(LocalDate.parse("2020-09-02") , 0.87, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-11") , 1.0, "education")
//				.restrict(LocalDate.parse("2020-09-09") , 0.82, "work")
//				.restrict(LocalDate.parse("2020-09-09") , 0.90, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-16") , 0.84, "work")
//				.restrict(LocalDate.parse("2020-09-16") , 0.91, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-23") , 0.83, "work")
//				.restrict(LocalDate.parse("2020-09-23") , 0.88, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-09-30") , 0.83, "work")
//				.restrict(LocalDate.parse("2020-09-30") , 0.87, "shop","leisure","other")
//				.restrict(LocalDate.parse("2020-10-07") , 0.81, "work")
//				.restrict(LocalDate.parse("2020-10-07") , 0.88, "shop","leisure","other");


		//------------------------------------------masks adoption--------------------------------------------------------
		//masks adoption is modeled as a Generalised logistic function

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
			restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResPt * 0.9, FaceMask.SURGICAL, logResPt * 0.1)), "pt interaction");
			restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResShop * 0.9, FaceMask.SURGICAL, logResShop * 0.1)), "shop","service");
			restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResWork * 0.9, FaceMask.SURGICAL, logResWork * 0.1)), "work");
			restrictions.restrict(date, Restriction.ofMask(Map.of(FaceMask.CLOTH, logResLeisure * 0.9, FaceMask.SURGICAL, logResLeisure * 0.1)), "leisure","other");
		}
		restrictions.restrict("2020-06-07", Restriction.ofMask(Map.of(FaceMask.CLOTH, 0.95 * 0.9, FaceMask.SURGICAL, 0.95 * 0.1)), "pt interaction");
		restrictions.restrict("2020-08-26", Restriction.ofMask(Map.of(FaceMask.CLOTH, 0.95 * 0.9, FaceMask.SURGICAL, 0.95 * 0.1)), "pt interaction","shop","service");
		return restrictions;
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
					break;
				case "ZUG":
					vehicleType.getCapacity().setSeats((int) (400 * capFactor));
					vehicleType.getCapacity().setStandingRoom((int) (0 * capFactor));
					break;
				default:
					throw new IllegalStateException("Unexpected value=|" + vehicleType.getId().toString() + "|");
			}
		}
		return scenario;
	}


	/*
	 *
	 * This class follow the Java Builder pattern that helps us to set up complex objects
	 * In this case we want to create a ConfigBuilder
	 */
	public static class BasePolicyBuilder {

		//Default values can be overwritten by using the below set methods
		private final EpisimConfigGroup episimConfig;
		private Map<String, Double> ciCorrections = Map.of("2020-03-12", 0.26);
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
				// we use (1) to set up our policy
				configBuilder = basePolicy(episimConfig, alpha, ciCorrections, extrapolation, introductionPeriod,
						maskCompliance);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
			return configBuilder;
		}
	}

	/*
	 * This method generates the config file that otherwise would be provided externally.
	 * This method is annotated with @Provides @Singleton, it means that the Guice Injector
	 * will use this method to create a Config instance and it will always use the same instance
	 * after the first initialization.
	 */

	@SuppressWarnings("deprecation")
	@Provides
	@Singleton
	public Config config() {

		//We use the method getBaseConfig() in the AbstractZurichScenario to create
		//an instance of Config that contains ContactIntensity and the progression model prob. dist.
		Config config = getBaseConfig();
		config.global().setRandomSeed(5672387461864L); //todo 10-15 seeds

		EpisimConfigGroup episimConfig = ConfigUtils.addOrGetModule(config, EpisimConfigGroup.class);

		episimConfig.setInputEventsFile(INPUT.resolve("output_events.xml.gz").toString());
		config.plans().setInputFile(INPUT.resolve("output_plans.xml.gz").toString());
		episimConfig.setSampleSize(1); // [0, 1]
		episimConfig.setInitialInfections(100); // todo unit: number of people
		episimConfig.setCalibrationParameter(1.05E-5); // todo
		episimConfig.setMaxContacts(4); // maximum number of people one can infect
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

		//ci correction (Seasonality)
		BasePolicyBuilder basePolicyBuilder = new BasePolicyBuilder(episimConfig);
		double ciCorrCorr = 0.65;
		basePolicyBuilder.setCiCorrections(Map.of(
				"2020-04-01",0.5*ciCorrCorr,
				"2020-07-15", 0.3*ciCorrCorr,
				"2020-09-01", 0.8*ciCorrCorr,
				"2020-09-15", 1.0*ciCorrCorr,
				"2020-11-05", 1.0*ciCorrCorr,
				"2021-01-08", 1.0*ciCorrCorr,
				"2021-02-23", 1.0*ciCorrCorr,
				"2021-03-20", 0.8*ciCorrCorr,
				"2021-04-01",0.6*ciCorrCorr,
				"2021-07-15", 0.3*ciCorrCorr
		));

		FixedPolicy.ConfigBuilder builder = basePolicyBuilder.build();
		episimConfig.setPolicy(FixedPolicy.class, builder.build());
		return config;
	}

}
