package org.matsim.episim.model;

import org.matsim.episim.EpisimConfigGroup;
import org.matsim.episim.EpisimPerson;
import org.matsim.episim.EpisimUtils;
import org.matsim.episim.TracingConfigGroup;
import com.google.inject.Inject;
import org.matsim.episim.model.progression.DiseaseStatusTransitionModel;

import java.util.SplittableRandom;
/**
 * Works exactly as the {@link ConfigurableProgressionModel}, but with age dependent transitions.
 */
public class AgeDependentProgressionModel extends ConfigurableProgressionModel {
	/**
	 * Constructor as in {@link ConfigurableProgressionModel}.
	 */
	@Inject
	public AgeDependentProgressionModel(SplittableRandom rnd, EpisimConfigGroup episimConfig, TracingConfigGroup tracingConfig, DiseaseStatusTransitionModel diseaseStatusTransitionModel) {
		super(rnd, episimConfig, tracingConfig, diseaseStatusTransitionModel);
	}

}
